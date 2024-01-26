use bevy::{
    asset::{DependencyLoadState, LoadState, RecursiveDependencyLoadState},
    ecs::schedule::SystemConfigs,
    prelude::*,
    utils::HashSet,
};
use bevy_dui::{DuiEntityCommandsExt, DuiPlugin, DuiProps, DuiRegistry};
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
        .add_systems(
            Update,
            StateTracker::<State>::transition_when_finished(State::Ready)
                .run_if(in_state(State::Loading)),
        )
        .add_systems(OnEnter(State::Ready), show_ui)
        .run();
}

fn load_assets(asset_server: Res<AssetServer>, mut tracker: ResMut<StateTracker<State>>) {
    // templates need to be loaded before use as the instantiation runs inline.
    tracker.load_asset(asset_server.load_folder("components"));
}

fn show_ui(mut commands: Commands, asset_server: Res<AssetServer>, dui: Res<DuiRegistry>) {
    commands.spawn(Camera3dBundle::default());

    commands
        .spawn(bevy_ecss::StyleSheet::new(
            asset_server.load("sheets/simple_ui.css"),
        ))
        .apply_template(&dui, "simple-ui", DuiProps::default())
        .unwrap();
}
