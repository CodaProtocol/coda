# SNAPP Credit Score Demo

## Dependencies:

```
yarn install
```

or

```
npm install
```

You will additionally need the `credit_score_demo.exe` binary to build SNAPPS.

You can compile the `credit_score_demo.exe` binary by doing the following:

1. Checkout to the `feature/snapp-demo-eth-addr` branch.

```
git checkout feature/snapp-demo-eth-addr
```

2.  Build the SNAPP demo client.

```
DUNE_PROFILE=testnet_postake_medium_curves dune build src/app/snapp_runner/examples/credit_score_demo/credit_score_demo.exe
```

3.  Once the binary is made, place it in `resources/bin`

You can additionally see `src/app/snapp_runner/examples/credit_score_demo/README.md` for more details on the SNAPP binary.

## Run

**Note:** If you are developing against this, change the `process.env.NODE_ENV` variable to `dev` in `main.js`.

```
yarn start
```

or

```
yarn dev
```

## Package for Linux

Building for Ubuntu 20.04 and Ubuntu 18.04 require different dependencies so you will have to build them differently. By default, this project is structured to build for Ubuntu 20.04. To build for 18.04, make the follow changes in `package.json`:

Change:

```
"depends": [
        "libjemalloc2",
        "libffi7",
        ...
      ]
```

to:

```
"depends": [
        "libjemalloc1",
        "libffi6",
        ...
      ]
```

Run the following to build an installer for Linux:

```
yarn dist
```

Make sure you have the `/resouces/bin/credit_score_demo.exe` in the project, otherwise the application will not behave as expected.
