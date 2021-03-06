/*
 * Licensed to Elasticsearch B.V. under one or more contributor
 * license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright
 * ownership. Elasticsearch B.V. licenses this file to you under
 * the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

'use strict'

const { writeFileSync } = require('fs')
const { join } = require('path')
const { types, endpoints } = require('../output/schema/schema.json')

let definitions = `/*
 * Licensed to Elasticsearch B.V. under one or more contributor
 * license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright
 * ownership. Elasticsearch B.V. licenses this file to you under
 * the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */\n\n`

// We don't skip `CommonQueryParameters` and `CommonCatQueryParameters`
// behaviors because we ue them for sharing common query parameters
// among most requests.
const skipBehaviors = [
  'ArrayResponseBase',
  'EmptyResponseBase',
  'AdditionalProperties'
]

for (const type of types) {
  if (type.kind === 'interface' && skipBehaviors.includes(type.name.name)) {
    continue
  }
  definitions += buildType(type) + '\n'
}

writeFileSync(
  join(__dirname, '..', 'output', 'typescript', 'types.ts'),
  definitions,
  'utf8'
)

function buildType (type) {
  switch (type.kind) {
    case 'instance_of':
      return createInstance(type)
    case 'array_of':
      return createArray(type)
    case 'union_of':
      return createUnionOf(type)
    case 'dictionary_of':
      return createDictionary(type)
    case 'named_value_of':
      return createNamedValue(type)
    case 'user_defined_value':
      return createUserDefinedValue(type)
    case 'interface':
      return createInterface(type)
    case 'request':
      return createRequest(type)
    case 'enum':
      return createEnum(type)
    case 'type_alias':
      return createAlias(type)
    case 'value':
      return buildType(type.value)
    case 'literal_value':
      return buildLiteralValue(type)
    default:
      throw new Error(`Unhandled kind: ${type.kind}`)
  }
}

function createInstance (type) {
  return `${type.type.name}${buildGenerics(type)}`
}

function createArray (type) {
  return `Array<${buildType(type.value)}>`
}

function createUnionOf (type) {
  return type.items.map(buildType).join(' | ')
}

function createDictionary (type) {
  return `Record<${buildType(type.key)}, ${buildType(type.value)}>`
}

function createNamedValue (type) {
  return `Record<string, ${buildType(type.value)}>`
}

function createUserDefinedValue () {
  return 'any'
}

function createInterface (type) {
  if (isSpecialInterface(type)) {
    return serializeSpecialInterface(type)
  }
  const isResponse = type.name.name.endsWith('Response')
  let code = `export interface ${type.name.name}${buildGenerics(type)}${buildInherits(type)} {\n`
  for (const property of type.properties) {
    code += `  ${cleanPropertyName(property.name)}${required(property)}: ${buildType(property.type)}\n`
    if (Array.isArray(property.aliases)) {
      for (const alias of property.aliases) {
        code += `  ${cleanPropertyName(alias)}${required(property)}: ${buildType(property.type)}\n`
      }
    }
  }
  code += '}\n'
  return code

  function required (property) {
    // response default to required, unless explicitly marked as optional
    if (isResponse) return property.required === false ? '?' : ''
    return property.required ? '' : '?'
  }
}

function createRequest (type) {
  // At the moment the only behaviors that request interfaces could have
  // are common query parameters that are shared among most apis.
  // Until a more complex case will happen, we can safely "push" the
  // common parameters in the inherits array.

  let code = `export interface ${type.name.name}${buildGenerics(type)}${buildInherits(type)} {\n`
  for (const property of type.path) {
    code += `  ${cleanPropertyName(property.name)}${property.required ? '' : '?'}: ${buildType(property.type)}\n`
  }

  // It might happen that the same property is present in both
  // path and query parameters, we should keep only one
  const pathPropertiesNames = type.path.map(property => property.name)
  for (const property of type.query) {
    if (pathPropertiesNames.includes(property.name)) continue
    code += `  ${cleanPropertyName(property.name)}${property.required ? '' : '?'}: ${buildType(property.type)}\n`
  }
  if (type.body && type.body.kind === 'properties') {
    code += `  body${isBodyRequired() ? '' : '?'}: {\n`
    for (const property of type.body.properties) {
      code += `    ${cleanPropertyName(property.name)}${property.required ? '' : '?'}: ${buildType(property.type)}\n`
    }
    code += '  }\n'
  } else if (type.body != null) {
    code += `  body${isBodyRequired() ? '' : '?'}: ${buildType(type.body)}\n`
  }
  code += '}\n'
  return code

  function isBodyRequired () {
    for (const endpoint of endpoints) {
      if (endpoint.request && endpoint.request.name === type.name.name) {
        return endpoint.requestBodyRequired
      }
    }
    return false
  }
}

function createEnum (type) {
  return `export type ${type.name.name} = ${type.members.map(m => `'${m.name}'`).join(' | ')}\n`
}

function createAlias (type) {
  return `export type ${type.name.name}${buildGenerics(type)} = ${buildType(type.type)}\n`
}

function buildInherits (type) {
  // since typescript is all arrays we can just merge
  const inherits = (type.inherits || []).filter(type => !skipBehaviors.includes(type.type.name))
  const interfaces = (type.implements || []).filter(type => !skipBehaviors.includes(type.type.name))
  const behaviors = (type.behaviors || []).filter(type => !skipBehaviors.includes(type.type.name))
  const extendAll = inherits.concat(interfaces).concat(behaviors)
  if (extendAll.length === 0) return ''
  return ` extends ${extendAll.map(buildInheritType).join(', ')}`

  function buildInheritType (type) {
    return `${type.type.name}${buildGenerics(type)}`
  }
}

function buildGenerics (type, noDefault = false) {
  if (!Array.isArray(type.generics)) return ''
  return `<${type.generics.map(buildGeneric).join(', ')}>`

  // generics can either be a value/instance_of or a named generics
  function buildGeneric (type) {
    return typeof type === 'string'
      ? (noDefault ? type : `${type} = unknown`)
      : buildType(type)
  }
}

function cleanPropertyName (name) {
  return name.includes('.') || name.includes('-') || name.match(/^(\d|\W)/)
    ? `'${name}'`
    : name
}

function isSpecialInterface (type) {
  if (Array.isArray(type.attachedBehaviors)) {
    // assume types ending with Base are abstract and are not the ones doing the implements
    if (type.name.name.endsWith('Base')) {
      return false
    }
    return type.attachedBehaviors.length > 0
  }
  switch (type.name.name) {
    case 'DictionaryResponseBase':
      return true
    default:
      return false
  }
}

function lookupBehavior (type, name) {
  if (!type.attachedBehaviors.includes(name)) return null
  if (Array.isArray(type.behaviors)) {
    const behavior = type.behaviors.find(i => i.type.name === name)
    if (behavior) return behavior
  }
  if (!type.inherits) return null
  const parentType = types.find(t => t.name.name === type.inherits[0].type.name)
  if (!parentType) return null
  return lookupBehavior(parentType, name)
}

function serializeAdditionalPropertiesType (type) {
  const dictionaryOf = lookupBehavior(type, 'AdditionalProperties')
  if (!dictionaryOf) throw new Error(`Unknown implements ${type.name.name}`)
  let code = `export interface ${type.name.name}Keys${buildGenerics(type)}${buildInherits(type)} {\n`

  function required (property) {
    return property.required ? '' : '?'
  }

  for (const property of type.properties) {
    code += `  ${cleanPropertyName(property.name)}${required(property)}: ${buildType(property.type)}\n`
  }
  code += '}\n'
  code += `export type ${type.name.name}${buildGenerics(type)} = ${type.name.name}Keys${buildGenerics(type, true)} |\n`
  code += `    { ${buildIndexer(dictionaryOf)} }\n`
  return code
}

function serializeSpecialInterface (type) {
  if (Array.isArray(type.attachedBehaviors)) {
    if (type.attachedBehaviors.includes('AdditionalProperties')) {
      return serializeAdditionalPropertiesType(type)
    }
    if (type.attachedBehaviors.includes('ArrayResponseBase')) {
      const behavior = lookupBehavior(type, 'ArrayResponseBase')
      let generic = behavior.generics[0].type.name
      if (generic === 'TCatRecord') {
        generic = type.inherits[0].generics[0].type.name
      }
      // In the input spec the Cat* responses are represented as an object
      // that contains a `records` key, which is an array of the inherited generic.
      // What ES actually sends back, is an array of the inherited generic.
      return `export type ${type.name.name} = ${generic}[]\n`
    }
    if (type.attachedBehaviors.includes('EmptyResponseBase')) {
      return `export type ${type.name.name} = boolean\n`
    }
  }

  switch (type.name.name) {
    case 'DictionaryResponseBase':
      return `export interface DictionaryResponseBase<TKey = unknown, TValue = unknown> extends ResponseBase {
  [key: string]: TValue
}\n`
    default:
      throw new Error(`Unknown interface ${type.name.name}`)
  }
}
function buildIndexer (type) {
  if (!Array.isArray(type.generics)) return ''
  return `[property: ${type.generics.map(buildGeneric).join(']: ')}`

  // generics can either be a value/instance_of or a named generics
  function buildGeneric (type) {
    const t = typeof type === 'string' ? type : buildType(type)
    // indexers do not allow type aliases so here we translate known
    // type aliases back to string
    return t === 'AggregateName' ? 'string' : t
  }
}

function buildLiteralValue (type) {
  if (typeof type.value === 'string') {
    return `'${type.value}'`
  }
  return type.value
}
