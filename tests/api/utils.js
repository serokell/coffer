// SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: MPL-2.0

const axios = require('axios')

let json_equal = (a, b) => JSON.stringify(a) == JSON.stringify(b)
let inspect    = (x)    => require('util').inspect(x, {depth: null})
let rand       = ()     => Math.round(Math.random() * 100000000)

let dumpAll = false

let method = (verb, {url, data, opts}) => {
  switch (verb) {
    case "GET":    return axios.get(url, opts)
    case "POST":   return axios.post(url, data, opts)
    case "DELETE": return axios.delete(url, opts)
    default:       throw Error(`Unsupported HTTP verb ${verb}`)
  }
}

let call = (verb) => ({opts, url, data, onSuccess, onError, done, dump}) => {
  dump      = dump      || dumpAll
  onSuccess = onSuccess || (r => Error(`Unexpected success ${r}`))
  onError   = onError   || (r => Error(`Unexpected error ${r}`))
  method(verb, {url, data, opts})
    .then(
      r => {
        if (dump) {
          console.log(inspect(r))
        }
        done(onSuccess(r))
      },
      e => {
        if (dump) {
          console.error(e)
        }
        done(onError(e))
      })
    .catch(done)
}

let GET = call("GET")
let POST = call("POST")
let DELETE = call("DELETE")

let all = (bools) => bools.every(x => x)

let explode = msg => { throw Error(inspect(msg)) }

var fromList = (list) =>
  list.length == 0 ? "origin" :
  list.length == 2 && list[0].length == 0 ? list[1].toString() :
  fromList(list[0]) + "." + list[1]

let contains = (part) => (whole) => {
  var loop = (crumbs, whole, part) => {
    // console.log(crumbs, whole, part)

    if (typeof whole !== typeof part && typeof part !== "function") {
      throw Error(`Different types at ${fromList(crumbs)}: ${inspect(whole)} :: ${typeof whole} !== ${inspect(part)} :: ${typeof part}`)
    }

    let missing = []

    switch (typeof part) {
      case "object":
        if (whole === part) { // null
          return
        }
        for (let key in part) {
          loop([crumbs, key], whole[key], part[key])
        }
        return

      case "string":
      case "number":
      case "undefined":
        if (whole !== part) {
          throw Error(`Different at ${fromList(crumbs)} : ${whole} !=== ${part}`)
        }
        return

      case "function": // predicate
        if (!part(whole)) {
          throw Error(`Predicate failed at ${fromList(crumbs)} : ${part.toString()} $ ${inspect(whole)}`)
        }
        return
    }
  }
  loop([], whole, part)
}

let noError = (t) => (x) => {
  try {
    t(x)
    return true
  } catch (e) {
    return false
  }
}

module.exports = {GET, POST, DELETE, all, contains, json_equal, rand, noError, explode }

// console.log(contains({"a": {"b": ["c", "d"]}, "e": "f"}, {"a": {"b": ["d"]}}))
