use anyhow::{anyhow, bail, ensure};
use bevy::{
    asset::{AssetLoader, AsyncReadExt},
    ecs::{
        component::Tick,
        reflect::ReflectCommandExt,
        system::{CommandQueue, EntityCommands},
    },
    prelude::*,
    text::TextLayoutInfo,
    ui::{
        widget::{TextFlags, UiImageSize},
        ContentSize, FocusPolicy,
    },
    utils::HashMap,
};
use bevy_ecss::{Property, PropertyValues};
use std::{
    any::{type_name, Any, TypeId},
    collections::BTreeMap,
};

/// pass properties into templates
#[derive(Default, Debug)]
pub struct DuiProps {
    props: HashMap<String, Box<dyn Any>>,
}

impl AsMut<DuiProps> for DuiProps {
    fn as_mut(&mut self) -> &mut DuiProps {
        self
    }
}

impl DuiProps {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_prop<T: 'static>(mut self, key: impl Into<String>, value: T) -> Self {
        self.insert_prop(key, value);
        self
    }

    pub fn insert_prop<T: 'static>(&mut self, key: impl Into<String>, value: T) {
        self.props.insert(key.into(), Box::new(value));
    }

    fn insert_prop_untyped(&mut self, key: impl Into<String>, value: Box<dyn Any>) {
        self.props.insert(key.into(), value);
    }

    /// returns Ok(Some(&value)) if the key is found and the value is of the correct type.
    ///
    /// returns Err(e) if the key is found but the value is not of the expected type.
    ///
    /// returns Ok(None) if the key is not found.
    pub fn borrow<T: 'static>(&mut self, key: &str) -> Result<Option<&T>, anyhow::Error> {
        self.props
            .get(key)
            .map(|b| {
                b.downcast_ref()
                    .ok_or_else(|| anyhow!("property {key} is not of type {}", type_name::<T>()))
            })
            .transpose()
    }

    /// returns Ok(Some(value)) if the key is found and the value is of the correct type.
    ///
    /// returns Err(e) if the key is found but the value is not of the expected type.
    ///
    /// returns Ok(None) if the key is not found.
    pub fn take<T: 'static>(&mut self, key: &str) -> Result<Option<T>, anyhow::Error> {
        self.props
            .remove(key)
            .map(|b| {
                b.downcast()
                    .map(|b| *b)
                    .map_err(|_| anyhow!("property {key} is not of type {}", type_name::<T>()))
            })
            .transpose()
    }

    fn take_untyped(&mut self, key: &str) -> Option<Box<dyn Any>> {
        self.props.remove(key)
    }

    fn extend(&mut self, other: DuiProps) {
        self.props.extend(other.props)
    }
}

pub type NodeMap = HashMap<String, Entity>;

/// root entity node and named nodes, returned from apply_template / spawn_template calls
pub struct DuiEntities {
    pub root: Entity,
    pub named_nodes: NodeMap,
}

impl DuiEntities {
    pub fn new(root: Entity) -> Self {
        Self {
            root,
            named_nodes: Default::default(),
        }
    }

    pub fn get_named(&self, name: &str) -> Option<Entity> {
        self.named_nodes.get(name).cloned()
    }

    pub fn named(&self, name: &str) -> Entity {
        self.named_nodes[name]
    }
}

/// trait for custom component implementations
pub trait DuiTemplate: Send + Sync {
    fn render(
        &self,
        commands: &mut EntityCommands,
        props: &mut DuiProps,
        dui_registry: &DuiRegistry,
    ) -> Result<NodeMap, anyhow::Error>;
}

#[derive(Asset, TypePath, Clone)]
pub struct DuiNodeList(Vec<DuiNode>);

#[derive(Clone)]
struct DuiNode {
    template: String,
    nodes: Vec<DuiElt>,
}

impl DuiNode {
    fn render_inner(
        commands: &mut EntityCommands,
        iter: &mut impl Iterator<Item = DuiElt>,
        props: &mut DuiProps,
        dui_registry: &DuiRegistry,
    ) -> Result<NodeMap, anyhow::Error> {
        let elt = iter.next().ok_or(anyhow!("unexpected end of iterator"))?;

        match elt {
            DuiElt::Node(NodeData {
                id,
                components,
                prop_components,
                children,
            }) => {
                for (ty, component) in components.iter() {
                    match ty {
                        ty if ty == &TypeId::of::<Text>() => {
                            let mut text_component = Text::default();
                            text_component.apply(component.as_ref());
                            if let Some(key) = prop_components.get(&PropComponent::Text) {
                                text_component.sections[0].value =
                                    props.take::<String>(key)?.unwrap_or_default();
                            }
                            debug!("added text_component");
                            commands.insert(text_component);
                        }
                        ty if ty == &TypeId::of::<UiImage>() => {
                            let mut ui_component = UiImage::default();
                            ui_component.apply(component.as_ref());
                            if let Some(key) = prop_components.get(&PropComponent::Image) {
                                ui_component.texture = dui_registry
                                    .asset_server()
                                    .load(props.take::<String>(key)?.unwrap_or_default());
                            }
                            debug!("added ui_image component");
                            commands.insert(ui_component);
                        }
                        _ => {
                            debug!(
                                "added reflect component of type {}",
                                Reflect::get_represented_type_info(component.as_ref())
                                    .unwrap()
                                    .type_path()
                            );
                            commands.insert_reflect(component.clone_value());
                        }
                    }
                }

                let mut result = Ok(HashMap::from_iter(id.map(|id| (id.clone(), commands.id()))));
                commands.with_children(|c| {
                    for _ in 0..children {
                        match Self::render_inner(&mut c.spawn_empty(), iter, props, dui_registry) {
                            Ok(components) => {
                                result.as_mut().unwrap().extend(components);
                            }
                            Err(e) => {
                                result = Err(e);
                                return;
                            }
                        }
                    }
                });

                result
            }
            DuiElt::Component {
                id,
                template,
                properties,
                passthrough,
            } => {
                let template = match &template {
                    PropValue::Literal(val) => val,
                    PropValue::Prop(key) => props
                        .borrow::<String>(key)?
                        .ok_or_else(|| anyhow!("missing property for template {key}"))?,
                };
                debug!("[{template}]");

                let component = dui_registry
                    .templates
                    .get(template)
                    .ok_or(anyhow!("missing component registry for `{template}`"))?;

                let mut component_props = DuiProps::new();
                for (k, v) in properties.iter() {
                    match v {
                        PropValue::Literal(lit) => {
                            debug!("-> prop {k} -> `{lit}`");
                            component_props.insert_prop(k.clone(), lit.clone())
                        }
                        PropValue::Prop(parent_key) => {
                            if let Some(val) = props.take_untyped(parent_key) {
                                debug!("[-> prop {k} -> @{parent_key} == `{val:?}`");
                                component_props.insert_prop_untyped(k.clone(), val);
                            } else {
                                debug!("[-> prop {k} -> @`{parent_key}` missing");
                                debug!("all props: {props:#?}");
                            }
                        }
                    }
                }

                let props_ref = if passthrough {
                    props.extend(component_props);
                    props
                } else {
                    &mut component_props
                };

                let component_id = commands.id();
                let mut result = component.render(commands, props_ref, dui_registry);
                match &mut result {
                    Ok(ref mut components) => {
                        if let Some(id) = id.as_ref() {
                            components.insert(id.clone(), component_id);
                        }
                    }
                    Err(_) => (),
                };
                result
            }
        }
    }
}

impl DuiTemplate for DuiNode {
    fn render(
        &self,
        commands: &mut EntityCommands,
        props: &mut DuiProps,
        dui_registry: &DuiRegistry,
    ) -> Result<NodeMap, anyhow::Error> {
        let mut iter = self.nodes.clone().into_iter();
        Self::render_inner(commands, &mut iter, props, dui_registry)
    }
}

/// resource storing templates, used to create instances
#[derive(Resource)]
pub struct DuiRegistry {
    templates: HashMap<String, Box<dyn DuiTemplate>>,
    asset_server: AssetServer,
}

impl FromWorld for DuiRegistry {
    fn from_world(world: &mut World) -> Self {
        let asset_server = world.resource::<AssetServer>().clone();
        Self {
            templates: default(),
            asset_server,
        }
    }
}

impl DuiRegistry {
    pub fn register_template(
        &mut self,
        name: impl Into<String>,
        template: impl DuiTemplate + 'static,
    ) {
        let name = name.into();
        debug!("added template {name}");
        self.templates.insert(name, Box::new(template));
    }

    pub fn apply_template(
        &self,
        commands: &mut EntityCommands,
        template: &str,
        mut props: impl AsMut<DuiProps>,
    ) -> Result<DuiEntities, anyhow::Error> {
        let named_nodes = self
            .templates
            .get(template)
            .ok_or(anyhow!("template `{template}` not found"))?
            .render(commands, props.as_mut(), self)?;
        Ok(DuiEntities {
            root: commands.id(),
            named_nodes,
        })
    }

    pub fn asset_server(&self) -> &AssetServer {
        &self.asset_server
    }
}

#[derive(Clone)]
pub enum PropValue {
    Literal(String),
    Prop(String),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum PropComponent {
    Text,
    Image,
    Template,
}

#[derive(Default)]
pub struct NodeData {
    id: Option<String>,
    components: BTreeMap<TypeId, Box<dyn Reflect>>,
    prop_components: HashMap<PropComponent, String>,
    children: usize,
}

pub enum DuiElt {
    Node(NodeData),
    Component {
        id: Option<String>,
        template: PropValue,
        properties: HashMap<String, PropValue>,
        passthrough: bool,
    },
}

impl Clone for DuiElt {
    fn clone(&self) -> Self {
        match self {
            Self::Node(NodeData {
                id,
                components,
                prop_components,
                children,
            }) => Self::Node(NodeData {
                id: id.clone(),
                components: BTreeMap::from_iter(
                    components.iter().map(|(k, v)| (*k, v.clone_value())),
                ),
                prop_components: prop_components.clone(),
                children: *children,
            }),
            Self::Component {
                id,
                template,
                properties,
                passthrough,
            } => Self::Component {
                id: id.clone(),
                template: template.clone(),
                properties: properties.clone(),
                passthrough: *passthrough,
            },
        }
    }
}

macro_rules! apply_prop {
    ($key:ident, $value:ident, $world:ident, $asset_server:ident, $components:ident, $T:ty) => {
        if $key == <$T>::name() {
            let mut queue = CommandQueue::default();
            let mut commands = Commands::new(&mut queue, $world);
            let reflected_component = $components.entry(TypeId::of::<<<$T as Property>::Components as std::ops::Deref>::Target>()).or_insert_with(|| {
                let val = <<<$T as Property>::Components as std::ops::Deref>::Target as Default>::default();
                Box::new(val).into_reflect()
            });
            let target = reflected_component.as_any_mut().downcast_mut().unwrap();
            let mut ta = Tick::new(0);
            let mut tb = Tick::new(0);
            let mut_wrapper = Mut::new(target, &mut ta, &mut tb, Tick::new(0), Tick::new(0));
            if let Ok(val) = <$T as Property>::parse($value) {
                <$T>::apply(&val, mut_wrapper, $asset_server, &mut commands);
            } else {
                warn!("failed to parse {} property value {:?}", std::any::type_name::<$T>(), $value);
            };
            true
        } else {
            false
        }
    }
}

macro_rules! apply_entity_prop {
    ($key:ident, $value:ident, $world:ident, $asset_server:ident, $entity:ident, $components:ident, $T:ty, $C:ty) => {
        if $key == <$T>::name() {
            let mut queue = CommandQueue::default();
            let mut commands = Commands::new(&mut queue, $world);
            if let Ok(val) = <$T as Property>::parse($value) {
                <$T>::apply(&val, $entity, $asset_server, &mut commands);
            } else {
                warn!(
                    "failed to parse {} property value {:?}",
                    std::any::type_name::<$T>(),
                    $value
                );
            };
            queue.apply($world);
            let component = $world.get::<$C>($entity).unwrap();
            $components.insert(
                TypeId::of::<$C>(),
                Box::new(component.clone()).into_reflect(),
            );
            true
        } else {
            false
        }
    };
}

impl DuiLoader {
    fn apply_props(
        k: &str,
        v: &PropertyValues,
        w: &mut World,
        e: Entity,
        a: &AssetServer,
        c: &mut BTreeMap<TypeId, Box<dyn Reflect>>,
    ) -> bool {
        use bevy_ecss::property::impls::*;

        apply_prop!(k, v, w, a, c, DisplayProperty)
            || apply_prop!(k, v, w, a, c, PositionTypeProperty)
            || apply_prop!(k, v, w, a, c, DirectionProperty)
            || apply_prop!(k, v, w, a, c, FlexDirectionProperty)
            || apply_prop!(k, v, w, a, c, FlexWrapProperty)
            || apply_prop!(k, v, w, a, c, AlignItemsProperty)
            || apply_prop!(k, v, w, a, c, AlignSelfProperty)
            || apply_prop!(k, v, w, a, c, AlignContentProperty)
            || apply_prop!(k, v, w, a, c, JustifyContentProperty)
            || apply_prop!(k, v, w, a, c, OverflowAxisXProperty)
            || apply_prop!(k, v, w, a, c, OverflowAxisYProperty)
            || apply_prop!(k, v, w, a, c, LeftProperty)
            || apply_prop!(k, v, w, a, c, RightProperty)
            || apply_prop!(k, v, w, a, c, TopProperty)
            || apply_prop!(k, v, w, a, c, BottomProperty)
            || apply_prop!(k, v, w, a, c, WidthProperty)
            || apply_prop!(k, v, w, a, c, HeightProperty)
            || apply_prop!(k, v, w, a, c, MinWidthProperty)
            || apply_prop!(k, v, w, a, c, MinHeightProperty)
            || apply_prop!(k, v, w, a, c, MaxWidthProperty)
            || apply_prop!(k, v, w, a, c, MaxHeightProperty)
            || apply_prop!(k, v, w, a, c, FlexBasisProperty)
            || apply_prop!(k, v, w, a, c, FlexGrowProperty)
            || apply_prop!(k, v, w, a, c, FlexShrinkProperty)
            || apply_prop!(k, v, w, a, c, AspectRatioProperty)
            || apply_prop!(k, v, w, a, c, MarginProperty)
            || apply_prop!(k, v, w, a, c, PaddingProperty)
            || apply_prop!(k, v, w, a, c, BorderProperty)
            || apply_prop!(k, v, w, a, c, FontColorProperty)
            || apply_prop!(k, v, w, a, c, FontProperty)
            || apply_prop!(k, v, w, a, c, FontSizeProperty)
            || apply_prop!(k, v, w, a, c, TextAlignProperty)
            || apply_prop!(k, v, w, a, c, TextContentProperty)
            || apply_prop!(k, v, w, a, c, ImageProperty)
            || apply_entity_prop!(k, v, w, a, e, c, BackgroundColorProperty, BackgroundColor)
            || apply_entity_prop!(k, v, w, a, e, c, BorderColorProperty, BorderColor)
    }

    fn node_from_attrs(
        e: &quick_xml::events::BytesStart,
        pos: usize,
        asset_server: &AssetServer,
        node_data: &mut NodeData,
    ) -> Result<(), anyhow::Error> {
        let NodeData {
            ref mut components,
            ref mut id,
            ref mut prop_components,
            ..
        } = node_data;

        macro_rules! ensure {
            ($c:ident, $t:ty) => {
                $c.entry(TypeId::of::<$t>())
                    .or_insert_with(|| Box::<$t>::default().into_reflect());
            };
        }

        ensure!(components, Node);
        ensure!(components, Style);
        ensure!(components, FocusPolicy);
        ensure!(components, Transform);
        ensure!(components, GlobalTransform);
        ensure!(components, Visibility);
        ensure!(components, InheritedVisibility);
        ensure!(components, ViewVisibility);
        ensure!(components, ZIndex);

        let mut world = World::new();
        let ent = world.spawn_empty().id();

        // process text first so that styles get applied correctly
        if let Some(attr) = e.try_get_attribute("text")? {
            let text_body = if attr.value.starts_with(b"@") {
                prop_components.insert(
                    PropComponent::Text,
                    String::from_utf8_lossy(&attr.value[1..]).into_owned(),
                );
                String::from("lorem ipsum")
            } else {
                String::from_utf8_lossy(&attr.value).into_owned()
            };

            components.insert(
                TypeId::of::<Text>(),
                Box::new(Text::from_section(text_body, Default::default())).into_reflect(),
            );
            ensure!(components, TextFlags);
            ensure!(components, TextLayoutInfo);
            ensure!(components, ContentSize);
        }

        for attr in e.attributes() {
            let attr = attr?;
            match attr.key.as_ref() {
                b"class" => {
                    components.insert(
                        TypeId::of::<bevy_ecss::Class>(),
                        Box::new(bevy_ecss::Class::new(
                            String::from_utf8_lossy(&attr.value).into_owned(),
                        ))
                        .into_reflect(),
                    );
                }
                b"id" => {
                    let name = String::from_utf8_lossy(&attr.value).into_owned();
                    *id = Some(name.clone());
                    components.insert(
                        TypeId::of::<Name>(),
                        Box::new(Name::new(name)).into_reflect(),
                    );
                }
                b"style" => {
                    let content = format!("#inline {{{}}}", std::str::from_utf8(&attr.value)?);
                    let ss = bevy_ecss::StyleSheetAsset::parse("", &content);
                    for rule in ss.iter() {
                        for (key, value) in rule.properties.iter() {
                            if !Self::apply_props(
                                key,
                                value,
                                &mut world,
                                ent,
                                asset_server,
                                components,
                            ) {
                                warn!("unmatched style property {key}");
                            }
                        }
                    }
                }
                b"text" => (), // already handled above
                b"image" => {
                    if attr.value.as_ref().starts_with(b"@") {
                        prop_components.insert(
                            PropComponent::Image,
                            String::from_utf8_lossy(&attr.value[1..]).into_owned(),
                        );
                    } else {
                        let image =
                            asset_server.load(String::from_utf8_lossy(&attr.value).into_owned());
                        components.insert(
                            TypeId::of::<UiImage>(),
                            Box::new(UiImage::new(image)).into_reflect(),
                        );
                    }
                }
                b"focus" => {
                    match attr.value.as_ref() {
                        b"pass" => components.insert(
                            TypeId::of::<FocusPolicy>(),
                            Box::new(FocusPolicy::Pass).into_reflect(),
                        ),
                        b"block" => components.insert(
                            TypeId::of::<FocusPolicy>(),
                            Box::new(FocusPolicy::Block).into_reflect(),
                        ),
                        _ => bail!("focus policy must be 'block' or 'pass'"),
                    };
                }
                b"interact" => {
                    components.insert(
                        TypeId::of::<Interaction>(),
                        Box::<Interaction>::default().into_reflect(),
                    );
                }
                b"z-index" => {
                    let index = String::from_utf8_lossy(attr.value.as_ref()).parse::<i32>()?;
                    components.insert(
                        TypeId::of::<ZIndex>(),
                        Box::new(ZIndex::Local(index)).into_reflect(),
                    );
                }
                _ => bail!("unexpected attribute {:?} @ {}", attr.key.as_ref(), pos),
            }
        }

        // overwrite bg color if not explicitly specified
        if components.get(&TypeId::of::<BackgroundColor>()).is_none() {
            // yick
            let default_bg = if components.contains_key(&TypeId::of::<UiImage>()) {
                Color::WHITE
            } else {
                Color::NONE
            };
            components
                .entry(TypeId::of::<BackgroundColor>())
                .or_insert_with(|| Box::new(BackgroundColor::from(default_bg)).into_reflect());
        }
        // make sure images get the other required ImageBundle components
        if components.contains_key(&TypeId::of::<UiImage>()) {
            ensure!(components, ContentSize);
            ensure!(components, UiImageSize);
        }

        Ok(())
    }

    fn read_inner(
        xml: &mut quick_xml::Reader<&[u8]>,
        asset_server: &AssetServer,
    ) -> Result<Option<DuiNode>, anyhow::Error> {
        let mut template = String::default();
        let mut elts = Vec::default();
        let mut open_elts: Vec<(usize, quick_xml::events::BytesStart<'static>)> = Vec::default();

        loop {
            let pos = xml.buffer_position();
            let ev = xml.read_event()?;
            match &ev {
                quick_xml::events::Event::Eof => {
                    if !template.is_empty() {
                        return Err(anyhow!("malformed template"));
                    }

                    return Ok(None);
                }

                quick_xml::events::Event::Empty(e) | quick_xml::events::Event::Start(e) => {
                    let elt = match e.name().as_ref() {
                        b"define-template" => {
                            ensure!(template.is_empty());
                            template = String::from_utf8_lossy(
                                &e.try_get_attribute("id")?
                                    .ok_or(anyhow!(
                                        "define-template must have an `id` attribute @ {pos}"
                                    ))?
                                    .value,
                            )
                            .into_owned();
                            None
                        }
                        b"div" => {
                            let mut node_data = NodeData::default();
                            if matches!(ev, quick_xml::events::Event::Empty(_)) {
                                Self::node_from_attrs(e, pos, asset_server, &mut node_data)?
                            }
                            Some(DuiElt::Node(node_data))
                        }
                        b"component" => {
                            let target_val = &e
                                .try_get_attribute("template")?
                                .ok_or(anyhow!(
                                    "define-template must have a `template` attribute @ {pos}"
                                ))?
                                .value;
                            let target = if target_val.starts_with(b"@") {
                                PropValue::Prop(
                                    String::from_utf8_lossy(&target_val[1..]).into_owned(),
                                )
                            } else {
                                PropValue::Literal(String::from_utf8_lossy(target_val).into_owned())
                            };
                            let properties = HashMap::from_iter(
                                e.attributes().flat_map(|a| a.ok()).map(|attr| {
                                    let target_prop =
                                        String::from_utf8_lossy(attr.key.as_ref()).into_owned();
                                    let value = if attr.value.starts_with(&[b'@']) {
                                        PropValue::Prop(
                                            String::from_utf8_lossy(&attr.value[1..]).into_owned(),
                                        )
                                    } else {
                                        PropValue::Literal(
                                            String::from_utf8_lossy(&attr.value).into_owned(),
                                        )
                                    };

                                    (target_prop, value)
                                }),
                            );
                            let id = e
                                .try_get_attribute("id")?
                                .map(|attr| String::from_utf8_lossy(&attr.value).into_owned());
                            let passthrough = e.try_get_attribute("passthrough")?.is_some();
                            Some(DuiElt::Component {
                                id,
                                template: target,
                                properties,
                                passthrough,
                            })
                        }
                        _ => bail!("unexpected tag {e:?} @ {pos}"),
                    };
                    ensure!(!template.is_empty());

                    if let Some((ix, _)) = open_elts.last() {
                        match elts[*ix] {
                            Some(DuiElt::Node(NodeData {
                                ref mut children, ..
                            })) => *children += 1,
                            None => (),
                            _ => bail!("unexpected child element @ {pos}"),
                        }
                    }

                    if matches!(ev, quick_xml::events::Event::Start(_)) {
                        open_elts.push((elts.len(), e.clone().into_owned()));
                    }

                    elts.push(elt);
                }

                quick_xml::events::Event::Text(e) => {
                    let Some(Some(DuiElt::Node(NodeData {
                        ref mut components, ..
                    }))) = elts.last_mut()
                    else {
                        bail!("text not expected here @ {}", pos);
                    };
                    components.insert(
                        TypeId::of::<Text>(),
                        Box::new(Text::from_section(
                            e.unescape()?.into_owned(),
                            Default::default(),
                        ))
                        .into_reflect(),
                    );
                    components.insert(
                        TypeId::of::<TextLayoutInfo>(),
                        Box::<TextLayoutInfo>::default().into_reflect(),
                    );
                    components.insert(
                        TypeId::of::<TextFlags>(),
                        Box::<TextFlags>::default().into_reflect(),
                    );
                    components.insert(
                        TypeId::of::<ContentSize>(),
                        Box::<ContentSize>::default().into_reflect(),
                    );
                }

                quick_xml::events::Event::End(_) => {
                    let (ix, e) = open_elts.pop().ok_or(anyhow!("close without open"))?;
                    let Some(DuiElt::Node(ref mut node_data)) = elts[ix] else {
                        // closing the define-template
                        let None = elts.remove(0) else {
                            panic!();
                        };
                        return Ok(Some(DuiNode {
                            template,
                            nodes: elts.into_iter().map(Option::unwrap).collect(),
                        }));
                    };
                    Self::node_from_attrs(&e, pos, asset_server, node_data)?;
                }

                // skip
                quick_xml::events::Event::Comment(_)
                | quick_xml::events::Event::Decl(_)
                | quick_xml::events::Event::PI(_)
                | quick_xml::events::Event::CData(_)
                | quick_xml::events::Event::DocType(_) => (),
            }
        }
    }
}

pub struct DuiLoader {
    asset_server: AssetServer,
}

impl AssetLoader for DuiLoader {
    type Asset = DuiNodeList;

    type Settings = ();

    type Error = anyhow::Error;

    fn load<'a>(
        &'a self,
        reader: &'a mut bevy::asset::io::Reader,
        _: &'a Self::Settings,
        context: &'a mut bevy::asset::LoadContext,
    ) -> bevy::utils::BoxedFuture<'a, Result<Self::Asset, Self::Error>> {
        Box::pin(async move {
            let res = async {
                let mut buf = Vec::default();
                reader.read_to_end(&mut buf).await?;
                let mut xml = quick_xml::Reader::from_str(std::str::from_utf8(&buf)?);
                xml.trim_text(true);

                let mut list = Vec::default();
                while let Some(node) = Self::read_inner(&mut xml, &self.asset_server)? {
                    list.push(node);
                }

                Ok(DuiNodeList(list))
            }
            .await;
            if let Err(e) = &res {
                warn!("error processing dui template `{:?}`: {e}", context.path());
            }
            res
        })
    }

    fn extensions(&self) -> &[&str] {
        &["dui"]
    }
}

pub struct DuiPlugin;

impl Plugin for DuiPlugin {
    fn build(&self, app: &mut App) {
        let asset_server = app.world.resource::<AssetServer>().clone();
        app.init_asset::<DuiNodeList>()
            .register_asset_loader(DuiLoader { asset_server })
            .init_resource::<DuiRegistry>()
            .add_systems(PreUpdate, add_duis);
    }
}

fn add_duis(
    mut evs: EventReader<AssetEvent<DuiNodeList>>,
    assets: Res<Assets<DuiNodeList>>,
    mut registry: ResMut<DuiRegistry>,
) {
    for ev in evs.read() {
        match ev {
            AssetEvent::Added { id } | AssetEvent::Modified { id } => {
                let Some(list) = assets.get(*id) else {
                    continue;
                };
                for node in list.0.iter() {
                    registry.register_template(node.template.clone(), node.clone());
                }
            }
            _ => (),
        }
    }
}

pub trait DuiCommandsExt {
    fn spawn_template(
        &mut self,
        registry: &DuiRegistry,
        template: &str,
        props: impl AsMut<DuiProps>,
    ) -> Result<DuiEntities, anyhow::Error>;
}

impl<'w, 's> DuiCommandsExt for Commands<'w, 's> {
    fn spawn_template(
        &mut self,
        registry: &DuiRegistry,
        template: &str,
        props: impl AsMut<DuiProps>,
    ) -> Result<DuiEntities, anyhow::Error> {
        self.spawn_empty().apply_template(registry, template, props)
    }
}

impl<'w, 's, 'a> DuiCommandsExt for ChildBuilder<'w, 's, 'a> {
    fn spawn_template(
        &mut self,
        registry: &DuiRegistry,
        template: &str,
        props: impl AsMut<DuiProps>,
    ) -> Result<DuiEntities, anyhow::Error> {
        let mut root = self.spawn_empty();
        registry.apply_template(&mut root, template, props)
    }
}

pub trait DuiEntityCommandsExt {
    fn apply_template(
        &mut self,
        registry: &DuiRegistry,
        template: &str,
        props: impl AsMut<DuiProps>,
    ) -> Result<DuiEntities, anyhow::Error>;
}

impl<'w, 's, 'a> DuiCommandsExt for EntityCommands<'w, 's, 'a> {
    fn spawn_template(
        &mut self,
        registry: &DuiRegistry,
        template: &str,
        props: impl AsMut<DuiProps>,
    ) -> Result<DuiEntities, anyhow::Error> {
        let results = self.commands().spawn_template(registry, template, props)?;
        self.push_children(&[results.root]);
        Ok(results)
    }
}

impl<'w, 's, 'a> DuiEntityCommandsExt for EntityCommands<'w, 's, 'a> {
    fn apply_template(
        &mut self,
        registry: &DuiRegistry,
        template: &str,
        props: impl AsMut<DuiProps>,
    ) -> Result<DuiEntities, anyhow::Error> {
        registry.apply_template(self, template, props)
    }
}
