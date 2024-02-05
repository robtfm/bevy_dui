a basic data-driven ui for bevy, for faster iteration (without compiling) and hot-reloading.

basic example:
```rs
fn show_ui(mut commands: Commands, asset_server: Res<AssetServer>, dui: Res<DuiRegistry>) {
    commands.spawn(Camera3dBundle::default());

    commands
        .spawn(bevy_ecss::StyleSheet::new(
            asset_server.load("sheets/simple_ui.css"),
        ))
        .apply_template(&dui, "simple-ui", DuiProps::default())
        .unwrap();
}
```

along with an xml [.dui file](assets/components/simple-ui.dui) generates a ui. this is the same result as [bevy_ecss's simple_ui](https://github.com/afonsolage/bevy_ecss/blob/main/examples/simple_ui.rs) example.

supports recursive embedded templates, and rust-implemented components which can take any data as properties. for example, the list of items (`right-moving-panel`) in the example above can be replaced with a template:
```html
    <div id="right-list" style="
        flex-direction: column-reverse;
        align-self: center;
        width: 100%;
        height: 50%;
        overflow-y: hidden;
        background-color: #191919;
    ">
        <component template="list" items="@list-items" id="rl-component" />
    </div>
```
and the values passed via `DuiProps`:
```rs
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
```

the code for the `list` template is in the [component-ui example](examples/component_ui.rs), `MyListTemplate` struct. Note that `apply_template` (and `spawn_template`) take a `DuiProps` argument, which allows key-value pairs to be passed to components. `.dui` files can use these in place of text and image sources (for which the values must be strings), or pass them to embedded components with `tag="@key"` attributes (any type of values are allowed here).

the crate makes heavy use of stylesheet parsing and component mappings from [bevy_ecss](https://github.com/afonsolage/bevy_ecss) (using a custom fork which `pub`s some of the guts). template instances interoperate with bevy_ecss similarly to html+css, except that stylesheets take precedence over dui-div style attributes (due to stylesheets being applied by later systems).

supports hot reloading for the templates only: re-applying a template after reloading will load the new ui, but not affect any existing instances (we do this so that properties don't need to be `Clone`, e.g. for closures).

---

## Dui elements:

### `<define-template>`

defines a template. templates can also be defined from code by implementing the `DuiTemplate` trait.

#### attributes:
tag | type | description
--- | --- | ---
`id` | required | the name by which the template will be available in the registry, after the .dui file is loaded as an asset.

#### Example

```html
<define-template id="simple-ui">
    ...
<define-template />
```

### `<div>` 

adds an entity with (at least) a `NodeBundle`.

#### attributes:
tag | type | description
--- | --- | ---
class | optional | creates a `bevy_ecss::Class` component. has no explicit meaning for dui, but when a stylesheet is also added to the root node, affects the styles that will be applied.
id | optional | creates a bevy `Name` component. named components are returned from `apply_template` and `spawn_template` calls for adding extra components etc in code
style | optional | acts like an inline stylesheet. the contents are parsed and applied to the initial components. supports all attributes available in [bevy_ecss](https://afonsolage.github.io/bevy_ecss/guide_properties.html). Note that user-registered properties are NOT supported in this version! (i will probably add them when i have a need to)
text | optional | adds `TextBundle` components to the entity with the given text content. these may also be added using `<div>text inside the tag</div>`, but the attribute also allows properties to be used with `text="@property-name"`.
image | optional | adds `ImageBundle` components to the entity. note unlike with text, html `<img>` tags are not supported. the value must be a path to the target image file. properties can be used with image="@property-name"
focus | optional | `focus="block"` adds a `FocusPolicy::Block` component. `focus="pass"` adds a `FocusPolicy::Pass` component, but can be omitted since that's the default.
interact | optional | adds an `Interaction` component.
z-index | optional | `z-index="3"` adds a `ZIndex(3)` component.

### `<component>`

spawns a new entity and applies another named template on it. the template name is taken from the element name.

#### attributes:

tag | type | description
--- | --- | ---
id | optional | identifies the entity in the `DuiEntities` struct returned from `apply_template` and `spawn_template` calls
passthrough | optional | specifies that all properties available to this template should be passed to the called template.
other properties | optional | will be passed to the referenced template as properties. may be linked to a property with an @-prefixed value.

```html
    <dialog title="Confirm" body="@message" ok-action="@ok-action" />
```

creates an entity and applies the `dialog` template, with the `title` property set to "Confirm", `body` property set to input the `message` property and the `ok-action` set to the input `ok-action` property.

### `<apply>`

applies some custom code on the containing entity. helpers `DuiMarkerComponent` and `DuiValueComponent` can be used to support adding arbitrary components onto entities. see the `toggle-vis` template in the component_ui example.

tag | type | description
--- | --- | ---
template | required | specifies the name of the template to apply. may be linked to a property with an @-prefixed value.
passthrough | optional | specifies that all properties available to this template should be passed to the called template.
other properties | optional | will be passed to the referenced template as properties. may be linked to a property with an @-prefixed value.

```html
    <apply template="my-apply-template" />
```

## todo

- support user-defined style properties
- examples
