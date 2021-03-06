# elastic client generator

This repository contains the Elasticsearch request/response definitions in TypeScript,
you can find them inside [`/specification/specs`](./specification/specs).
The [`specification`](specification) folder contains a TypeScript program that compiles the entire definition
in a JSON representation that can be used for generating language clients.

This JSON representation is formally defined by [a set of TypeScript definitions (a meta-model)](specification/src/metamodel.ts) that also explains the various properties and their values.

## Prepare the environment

For generating the JSON representation and running the validation code you need
to install and configure Node.js in your development environment.

You can install Node.js with [`nvm`](https://github.com/nvm-sh/nvm):

```sh
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash
```

Once the installation is completed, install Node.js v14:

```sh
nvm install 14
```

## How to generate the JSON representation

```
# clone the project
$ git clone https://github.com/elastic/elastic-client-generator.git

# install the dependencies
$ npm install --prefix specification

# generate the JSON representation
$ npm run generate-schema --prefix specification

# the generated output can be found in ./output/schema/schema.json
$ cat output/schema/schema.json
```

### Structure of the JSON representation

The JSON representation is [formally defined as TypeScript definitions](./specification/src/metamodel.ts). Refer to them for the full details. It is an object with two top level keys:

```jsonc
{
  "types": [...],
  "endpoints": [...]
}
```

The first one, `types`, contains all the type definitions from the specification, such as
`IndexRequest` or `MainError`, while the second one, `endpoints`, contains every
endpoint of Elasticsearch and the respective type mapping. For example:

```jsonc
{
  "types": [{
    "kind": "request",
    "name": {
      "namespace": "document.single.index",
      "name": "IndexRequest"
    },
    "description": "The document",
    "annotations": {
      "type_stability": "stable"
    },
    "generics": [
      "TDocument"
    ],
    "inherits": [
      {
        "type": {
          "namespace": "common_abstractions.request",
          "name": "RequestBase"
        }
      }
    ],
    "path": [...],
    "query": [...],
    "body": {...}
  }, {
    "kind": "interface",
    "name": {
      "namespace": "document.single.index",
      "name": "IndexResponse"
    },
    "inherits": [
      {
        "type": {
          "namespace": "document.single",
          "name": "WriteResponseBase"
        }
      }
    ],
    "properties": []
  }],
  "endpoints": [{
    "name": "index",
    "description": "Creates or updates a document in an index.",
    "docUrl": "https://www.elastic.co/guide/en/elasticsearch/reference/master/docs-index_.html",
    "stability": "stable",
    "request": {
      "namespace": "document.single.index",
      "name": "IndexRequest"
    },
    "requestBodyRequired": true,
    "response": {
      "namespace": "document.single.index",
      "name": "IndexResponse"
    },
    "urls": [...]
  }]
}
```

The example above represents the [index](https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-index_.html)
request, inside the `endpoints` array you can find the API name and the type mappings under `request.name`
and `response.name`. The respective type definitons can be found inside the `types` array.

In some cases an endpoint might be defined, but there is no a type definition yet, in such case
the `request` and `response` value will be `null`.

## How to validate the specification

The specification is validated daily by the [client-flight-recorder](https://github.com/elastic/clients-flight-recorder) project.
The validation result can be found [here](https://github.com/elastic/clients-flight-recorder/blob/dev/recordings/types-validation/types-validation.md).

### Validate the specification in your machine

The following step only apply if you don't have `~/.elastic/github.token` in place.

Create GitHub token to allow authentication with [Vault](https://www.vaultproject.io/).
  * Go to https://github.com/settings/tokens.
  * Click `Generate new token`.
  * Give your token a name and make sure to click the `repo` and `read:org` scopes.
  * Create a file at `~/.elastic/github.token` and paste the GitHub token into it.
  * Change permissions on the file allow access only from the user.
     `chmod 600 ~/.elastic/github.token`

You can see [here](https://github.com/elastic/infra/tree/master/docs/vault#github-auth)
how to generate a token.

Once you have configured the environment, run the following commands:

```sh
git clone https://github.com/elastic/elastic-client-generator.git
git clone https://github.com/elastic/clients-flight-recorder.git

cd elastic-client-generator
./run-validations.sh
```

The last command above will install all the dependencies and run, download
the test recordings and finally validate the specification.
If you need to download the recordings again, run `PULL_LATEST=true ./run-validations.sh`.

You can validate a specific API with the `--api` option, same goes for `--request` and `--response`.
For example, the following command validates the index request api:

```js
./run-validations.sh --api index --request
```
The following command validates the index response api:

```js
./run-validations.sh --api index --response
```
The following command validates the index request and response api:

```js
./run-validations.sh --api index --request --response
```

Once you see the errors, you can fix the original definition in `/specification/specs`
and then run the command again until the types validator does not trigger any new error.
Finally open a pull request with your changes.

Namespaced APIs can be validated in the same way, for example:

```js
./run-validations.sh --api cat.health --request
```

## FAQ

### A specific property is not always present, how do I define it?

When you define a property the syntax is `propertyName: propertyType`.
By default a property is required to exist. If you know that a property will not
always be there, you can add a question mark just before the column:

```ts
propertyRequired: string
propertyOptional?: string
```

### A definition is missing, how do I add it?

See [here](./docs/add-new-api.md).

### A definition is not correct, how do I fix it?

All the definitons are inside `specifications/specs` folder, search the bad defintion and update it,
you can find above how to run the validation of the spec.

### An endpoint is missing, how do I add it?

See [here](./docs/add-new-api.md).

### An endpoint definition is not correct, how do I fix it?

All the endpoint definitons are inside `specifications/specs/_json_spec` folder, which contains a series of
JSON files taken directly from the Elasticsearch rest-api-spec.
You should copy from there the updated endpoint defintion and change it here.

### The validation in broken on GitHub but works on my machine!

Very likely the recordings on your machine are stale, you can download
the latest version with:

```sh
PULL_LATEST=true ./run-validations.sh
```

You should pull the latest change from the `client-flight-recorder` as well.

```sh
cd client-flight-recorder
git pull
```

### Where do I find the generated test?

Everytime you run the `run-validations` script, a series of test will be generated and dumped on disk.
You can find them in `clients-flight-recorder/scripts/types-validator/workbench`.
The content of this folder is a series of recorded responses from Elasticsearch wrapped inside an helper
that verifies if the type definiton is correct.

### Which editor should I use?

Any editor is fine, but to have a better development experience it should be configured
to work with TypeScript. [Visual Studio Code](https://code.visualstudio.com/) and
[IntelliJ IDEA](https://www.jetbrains.com/idea/) come with TypeScript support out of the box.

### Is there a complete example of the process?

Yes, take a look [here](./docs/validation-example.md).

### realpath: command not found

The validation script uses realpath which may be not present in your system.
If you are using MacOS, run the following command to fix the issue:

```sh
brew install coreutils
```

### The `recordings-dev` folder contains a zip file and not the `tmp-*` folders

Very likely your system does not have the `zip` command installed.
```sh
# on mac
brew install zip

# on linux
apt-get install -y zip
```

Then remove the content of `recordings-dev/elasticsearch/*` and run `PULL_LATEST=true ./run-validations.sh` again.

### I need to modify che compiler, help!

Take a look at the [compiler documentation](./docs/compiler.md).

## BirdsEye overview

The work of several repositories come together in this repository.
This diagram aims to sketch an overview of how different pieces connect

![overview.png](overview.png)
