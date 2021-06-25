# elm-hovercard

Renders a hovercard above or below a certain DOM element in [Elm](https://elm-lang.org).

Documentation

```elm
hovercard
    { maxWidth = 100
    , maxHeight = 100
    , borderColor = Color.black
    , backgroundColor = Color.lightBlue
    , borderWidth = 2
    }
    -- Browser.Dom.Element representing
    -- viewport and position of the element
    element
    [ div
        []
        [ text "Lorem ipsum dolor sit amet"
        ]
    ]
```

![image](https://user-images.githubusercontent.com/1172181/123420146-7694dc80-d5bb-11eb-99ef-cdb93b9b2ec4.png)


See `example/Example.elm`!
