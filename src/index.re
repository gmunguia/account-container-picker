[@bs.deriving abstract]
type defaultSuggestion = {description: string};

[@bs.deriving abstract]
type suggestion = {
  description: string,
  content: string,
};

[@bs.deriving abstract]
type contextualIdentity = {
  [@bs.as "cookieStoreId"]
  id: string,
  color: string,
  colorCode: string,
  icon: string,
  iconUrl: string,
  name: string,
};

[@bs.deriving abstract]
type createProperties = {
  [@bs.optional]
  active: bool,
  [@bs.optional]
  cookieStoreId: string,
  [@bs.optional]
  index: int,
  [@bs.optional]
  openerTabId: int,
  [@bs.optional]
  openInReaderMode: bool,
  [@bs.optional]
  pinned: bool,
  [@bs.optional]
  url: string,
  [@bs.optional]
  windowId: int,
};

[@bs.val] [@bs.scope ("browser", "omnibox")]
external setDefaultSuggestion: defaultSuggestion => unit = "";

[@bs.val] [@bs.scope ("browser", "omnibox", "onInputStarted")]
external onInputStarted: (unit => unit) => unit = "addListener";

[@bs.val] [@bs.scope ("browser", "omnibox", "onInputChanged")]
external onInputChanged:
  ((string, (. array(suggestion)) => unit) => unit) => unit =
  "addListener";

[@bs.val] [@bs.scope ("browser", "omnibox", "onInputEntered")]
external onInputEntered: ((string, string) => unit) => unit = "addListener";

[@bs.val] [@bs.scope ("browser", "contextualIdentities")]
external queryIdentities:
  Js.Dict.t(string) => Js.Promise.t(array(contextualIdentity)) =
  "query";

[@bs.val] [@bs.scope ("browser", "tabs")]
external createTab: createProperties => Js.Promise.t(unit) = "create";

let getIdentities: unit => Js.Promise.t(array(contextualIdentity)) =
  () => {
    queryIdentities(Js.Dict.empty());
  };

onInputStarted(() =>
  getIdentities()
  |> Js.Promise.then_(ids => Array.map(nameGet, ids) |> Js.Promise.resolve)
  |> Js.Promise.then_(ids =>
       Array.fold_left((a, b) => a ++ " " ++ b, "", ids)
       |> Js.Promise.resolve
     )
  |> Js.Promise.then_(ids => {
       setDefaultSuggestion(defaultSuggestion(~description=ids));
       Js.Promise.resolve();
     })
  |> ignore
);

onInputChanged((input, suggest) =>
  getIdentities()
  |> Js.Promise.then_(identities =>
       Array.map(
         identity =>
           suggestion(
             ~description=nameGet(identity),
             ~content=idGet(identity),
           ),
         identities,
       )
       |> Js.Promise.resolve
     )
  |> Js.Promise.then_(suggestions => {
       suggest(. suggestions);
       Js.Promise.resolve();
     })
  |> Js.Promise.catch(error => {
       Js.log2("Couldn't make suggestions:", error);
       Js.Promise.resolve();
     })
  |> ignore
);

onInputEntered((input, disposition) =>
  createProperties(~cookieStoreId=input, ()) |> createTab |> ignore
);
