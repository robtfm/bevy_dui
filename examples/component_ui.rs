use bevy::{
    asset::{DependencyLoadState, LoadState, RecursiveDependencyLoadState},
    ecs::{schedule::SystemConfigs, system::EntityCommands},
    prelude::*,
    utils::HashSet,
};
use bevy_dui::{DuiEntityCommandsExt, DuiPlugin, DuiProps, DuiRegistry, DuiTemplate, NodeMap};
// use bevy_dui::{Component, Css};
use std::marker::PhantomData;

#[derive(States, Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum State {
    #[default]
    Loading,
    Ready,
}

// load state tracker, not really important to the example.
#[derive(Resource, Default)]
pub struct StateTracker<S: States> {
    assets: HashSet<UntypedHandle>,
    _p: PhantomData<fn() -> S>,
}

impl<S: States> StateTracker<S> {
    pub fn load_asset<A: Asset>(&mut self, h: Handle<A>) {
        self.assets.insert(h.untyped());
    }

    pub fn transition_when_finished(next: S) -> SystemConfigs {
        let system = move |slf: Res<StateTracker<S>>,
                           asset_server: Res<AssetServer>,
                           mut next_state: ResMut<NextState<S>>| {
            if slf.assets.iter().all(|a| {
                asset_server.get_load_states(a.id())
                    == Some((
                        LoadState::Loaded,
                        DependencyLoadState::Loaded,
                        RecursiveDependencyLoadState::Loaded,
                    ))
            }) {
                next_state.set(next.clone())
            }
        };

        system.into_configs()
    }
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugins((bevy_ecss::EcssPlugin::default(), DuiPlugin))
        .add_state::<State>()
        .init_resource::<StateTracker<State>>()
        .add_systems(Startup, load_assets)
        .add_systems(Startup, register_components)
        .add_systems(
            Update,
            StateTracker::<State>::transition_when_finished(State::Ready)
                .run_if(in_state(State::Loading)),
        )
        .add_systems(OnEnter(State::Ready), show_ui)
        .add_systems(Update, toggle_vis.run_if(in_state(State::Ready)))
        .run();
}

fn load_assets(asset_server: Res<AssetServer>, mut tracker: ResMut<StateTracker<State>>) {
    // templates need to be loaded before use as the instantiation runs inline.
    tracker.load_asset(asset_server.load_folder("components"));
}

fn register_components(mut registry: ResMut<DuiRegistry>) {
    registry.register_template("list", MyListComponent);
}

#[derive(Component)]
pub struct ToggleVis;

fn show_ui(mut commands: Commands, asset_server: Res<AssetServer>, dui: Res<DuiRegistry>) {
    commands.spawn(Camera3dBundle::default());

    // we can use a custom component and pass properties to it. note that the type must match *exactly* (String is different to &str, etc).
    let list_items = (0..30).map(|i| format!("Item {i}")).collect::<Vec<_>>();

    let components = commands
        .spawn(bevy_ecss::StyleSheet::new(
            asset_server.load("sheets/simple_ui.css"),
        ))
        .apply_template(
            &dui,
            "component-ui",
            DuiProps::new().with_prop("list-items", list_items),
        )
        .unwrap();

    // we can add our own components to named nodes
    commands
        .entity(components.named("mid-red-last"))
        .insert(ToggleVis);
    commands
        .entity(components.named("rl-component"))
        .insert(ToggleVis);
}

fn toggle_vis(mut q: Query<&mut Visibility, With<ToggleVis>>, time: Res<Time>) {
    for mut v in q.iter_mut() {
        *v = if (time.elapsed_seconds() as u32) & 1 == 1 {
            Visibility::Hidden
        } else {
            Visibility::Inherited
        };
    }
}

pub struct MyListComponent;

impl DuiTemplate for MyListComponent {
    fn render<'w, 's, 'a>(
        &self,
        commands: &mut EntityCommands,
        props: &mut DuiProps,
        dui: &DuiRegistry,
    ) -> Result<NodeMap, anyhow::Error> {
        let items = props.take::<Vec<String>>("items")?.unwrap_or_default();
        let mut results = NodeMap::default();
        commands
            .insert(NodeBundle {
                style: Style {
                    flex_direction: FlexDirection::ColumnReverse,
                    flex_grow: 1.0,
                    ..Default::default()
                },
                ..Default::default()
            })
            .with_children(|c| {
                for (i, item) in items.into_iter().enumerate() {
                    let id = c
                        .spawn(
                            TextBundle::from_section(
                                format!("{item}"),
                                TextStyle {
                                    font: dui.asset_server().load("fonts/FiraSans-Bold.ttf"),
                                    font_size: 20.,
                                    color: Color::WHITE,
                                },
                            )
                            .with_style(Style {
                                flex_shrink: 0.,
                                height: Val::Px(20.),
                                margin: UiRect {
                                    left: Val::Auto,
                                    right: Val::Auto,
                                    ..default()
                                },
                                ..default()
                            }),
                        )
                        .insert(bevy_ecss::Class::new("big-text"))
                        .insert(Name::new(format!("item-{}", i)))
                        .id();

                    results.insert(format!("item-{}", i), id);
                }
            });
        Ok(results)
    }
}
