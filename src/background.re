[@bs.deriving abstract]
type suggestion = {
  description: string
};

[@bs.val] [@bs.scope ("browser", "omnibox")] external setDefaultSuggestion: suggestion => string = "";

setDefaultSuggestion(suggestion(~description="Test omnibox suggestion"))
