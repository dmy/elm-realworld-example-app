# ![Elm RealWorld Example App](logo.png)

> ### Elm codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API.


### [Demo](https://elm-realworld-example.netlify.app)
### [Documentation](https://elm-doc-preview.netlify.app/?repo=dmy/elm-realworld-example-app)

This codebase was created mainly to demonstrate a Single Page Application written in **Elm** using the **Effect pattern**. It was forked initialy from the great [rtfeldman/elm-spa-example](https://github.com/rtfeldman/elm-spa-example).

For more information on how this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.


# How it works
The Effect pattern used in this application consists in definining an [`Effect`](https://elm-doc-preview.netlify.app/Effect?repo=dmy%2Felm-realworld-example-app#Effect) custom type that can represent all the effects that `init` and `update` functions want to produce.

These effects can represent:
* a `Cmd` value
* a request to change the state at an upper level (for example an URL change from a subpage without an anchor)

The [`init`](https://elm-doc-preview.netlify.app/Main?repo=dmy%2Felm-realworld-example-app#init) and [`update`](https://elm-doc-preview.netlify.app/Main?repo=dmy%2Felm-realworld-example-app#update) functions are changed to use this new type, using a custom [`application`](https://elm-doc-preview.netlify.app/Effect?repo=dmy%2Felm-realworld-example-app#application) function, that turns the type into actual effects.

There are several benefits to this approach that makes it a valuable pattern for complex applications:
* All the effects are defined in a single [`Effect`](https://elm-doc-preview.netlify.app/Effect?repo=dmy%2Felm-realworld-example-app) module, which acts as an internal API for the whole application that is guaranteed to list every possible effect.
* Effects can be inspected and tested, not like `Cmd` values. This allows to test all the application effects, including HTTP requests. See [avh4/elm-program-test](https://github.com/avh4/elm-program-test) and its [section about testing commands](https://elm-program-test.netlify.app/cmds.html#testing-programs-with-cmds).
* Effects can represent a modification of top level model data, like the [Session](https://elm-doc-preview.netlify.app/Main?repo=dmy%2Felm-realworld-example-app#Model) when [logging in](https://elm-doc-preview.netlify.app/Effect?repo=dmy%2Felm-realworld-example-app#login), or the current page when an URL change is wanted by a subpage `update` function.
* All the `update` functions keep a clean and concise [`Msg -> Model -> ( Model, Effect Msg )`](https://github.com/dmy/elm-realworld-example-app/blob/master/src/Page/Home.elm#L140) signature.
* Because effects carry the minimum information required, some parameters like the `Browser.Navigation.key` are needed only in the effects [`perform`](https://github.com/dmy/elm-realworld-example-app/blob/master/src/Effect.elm#L209) function, which frees the developer from passing them to functions all over the application.
* A single `NoOp` or ([`Ignored String`](https://elm-doc-preview.netlify.app/Main?repo=dmy%2Felm-realworld-example-app#Msg)) can be used for the whole application.

# How to run

```bash
npm install
npm run build
npm run watch
```
