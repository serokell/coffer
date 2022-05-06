// SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
//
// SPDX-License-Identifier: MPL-2.0

let {GET, POST, DELETE, all, contains, json_equal, rand, noError, explode } = require("./utils")

let opts = {
  headers: {
    "token": "s.C6Wo4PfL69h0yotIaIa5xnoO",
    "Content-Type": 'application/json'
  }
}

describe('/api/v1/content', function () {
  let n = rand()
  let timestamp = Date(Date.now())
  console.log(timestamp)

  describe('/create', function () {
    it('is reachable', function (done) {
      POST({
        // dump: true,
        opts,
        url: `http://localhost:8081/api/v1/content/create?path=/foo${n}/bar`,
        data: [{"a": "b"}, {"c": "d"}],
        onSuccess: contains(
          { data:
            { tag: "CRSuccess"
            , contents:
              { ePath: {unEntryPath: [`foo${n}`, "bar"]}
              , eFields:
                { a: { fValue: "b", fVisibility: "public",  fDateModified: (date) => Date(date) >= timestamp }
                , c: { fValue: "d", fVisibility: "private", fDateModified: (date) => Date(date) >= timestamp }
                }
              , eTags: (tags) => json_equal(tags, [])
              , eDateModified: (date) => Date(date) >= timestamp
              }
            }
          }),
        done,
      })
    });

    it('tracks duplicates', function (done) {
      let timestamp = Date(Date.now())
      POST({
        // dump: true,
        opts,
        url: `http://localhost:8081/api/v1/content/create?path=/foo${n}/bar`,
        data: [{"a": "b"}, {"c" : "d"}],
        onSuccess: contains(
          { data:
            { tag: "CREntryAlreadyExists"
            , contents: {unEntryPath: [`foo${n}`, "bar"]}
            }
          }),
        done,
      })
    })

    it('can\'t create /', function (done) {
      let timestamp = Date(Date.now())
      POST({
        // dump: true,
        opts,
        url: `http://localhost:8081/api/v1/content/create?path=/`,
        data: [{"a": "b"}, {"c" : "d"}],
        onError: contains(
          { response: {data: 'Error parsing query parameter path failed: Entry paths must not be empty.'}}
        ),
        done,
      })
    })

    it('validates path slugs', function (done) {
      let timestamp = Date(Date.now())
      POST({
        // dump: true,
        opts,
        url: `http://localhost:8081/api/v1/content/create?path=/foo%bar`,
        data: [{"a": "b"}, {"c" : "d"}],
        onError: contains(
          { response: {data: "Error parsing query parameter path failed: Path segments can only contain the following characters: 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_'"}}
        ),
        done,
      })
    })
  });

  describe('/view', function () {
    it('shows stuff we\'ve just created at /', function (done) {
      GET({
        // dump: true,
        opts,
        url: "http://localhost:8081/api/v1/content/view?path=/",
        onSuccess: contains(
          { data:
            { contents:
              { dSubdirs:
                { [`foo${n}`]:
                  { dEntries: (dirs) => dirs.find(noError(contains(
                    { ePath: {unEntryPath: [`foo${n}`, "bar"]}
                    , eFields:
                      { a: { fValue: "b", fVisibility: "public",  fDateModified: (date) => Date(date) >= timestamp }
                      , c: { fValue: "d", fVisibility: "private", fDateModified: (date) => Date(date) >= timestamp }
                      }
                    , eTags: (tags) => json_equal(tags, [])
                    , eDateModified: (date) => Date(date) >= timestamp
                    }))),
                  }
                }
              }
            }
          }),
        done
      })
    });

    it('doesn\'t show path we didn\'t create', function (done) {
      GET({
        // dump: true,
        opts,
        url: `http://localhost:8081/api/v1/content/view?path=/foo${n}-nope/bar`,
        onSuccess: contains(
          { data:
            { tag: "VRPathNotFound"
            , contents: [`foo${n}-nope`, "bar"]
            }
          }
        ),
        done
      })
    });

    it('doesn\'t show item we didn\'t create #2', function (done) {
      GET({
        // dump: true,
        opts,
        url: `http://localhost:8081/api/v1/content/view?path=/foo${n}/qux`,
        onSuccess: contains(
          { data:
            { tag: "VRPathNotFound"
            , contents: [`foo${n}`, "qux"]
            }
          }
        ),
        done
      })
    });
  });

  describe('/set-field', function () {
    it('sets field', function (done) {
      let timestamp1 = Date(Date.now())
      POST({
        // dump: true,
        opts,
        url: `http://localhost:8081/api/v1/content/set-field?path=/foo${n}/bar&field=a`,
        data: "z",
        onSuccess: contains(
          { data:
            { contents:
              { ePath: {unEntryPath: [`foo${n}`, "bar"]}
              , eFields:
                { a: { fValue: "z", fVisibility: "public",  fDateModified: (date) => Date(date) >= timestamp1 }
                , c: { fValue: "d", fVisibility: "private", fDateModified: (date) => Date(date) >= timestamp }
                }
              , eTags: (tags) => json_equal(tags, [])
              , eDateModified: (date) => Date(date) >= timestamp1
              },
            }
          }),
        done,
      })
    })

    let timestamp2 = Date(Date.now())
    describe('/private', function () {
      it('sets field private', function (done) {
        POST({
          // dump: true,
          opts,
          url: `http://localhost:8081/api/v1/content/set-field/private?path=/foo${n}/bar&field=a`,
          data: "z",
          onSuccess: contains(
            { data:
              { contents:
                { ePath: {unEntryPath: [`foo${n}`, "bar"]}
                , eFields:
                  { a: { fValue: "z", fVisibility: "private", fDateModified: (date) => Date(date) >= timestamp2 }
                  , c: { fValue: "d", fVisibility: "private", fDateModified: (date) => Date(date) >= timestamp }
                  }
                , eTags: (tags) => json_equal(tags, [])
                , eDateModified: (date) => Date(date) >= timestamp2
                },
              }
            }),
          done,
        })
      })
    });

    describe('/public', function () {
      it('sets field public', function (done) {
        let timestamp3 = Date(Date.now())
        POST({
          // dump: true,
          opts,
          url: `http://localhost:8081/api/v1/content/set-field/public?path=/foo${n}/bar&field=c`,
          data: "z",
          onSuccess: contains(
            { data:
              { contents:
                { ePath: { unEntryPath: [`foo${n}`, "bar"] } // ???
                , eFields:
                  { a: { fValue: "z", fVisibility: "private", fDateModified: (date) => Date(date) >= timestamp2 }
                  , c: { fValue: "d", fVisibility: "public",  fDateModified: (date) => Date(date) >= timestamp3 }
                  }
                , eTags: (tags) => json_equal(tags, [])
                , eDateModified: (date) => Date(date) >= timestamp3
                },
              }
            }),
            done,
        })
      })
    });
  });

  describe("/find", function () {
    it('finds a subdir', function (done) {
      GET({
        // dump: true,
        opts,
        url: `http://localhost:8081/api/v1/content/find?path=/foo${n}/bar&text=a`,
        onSuccess: contains(
          { data:
            { dSubdirs:
              { [`foo${n}`]:
                { dEntries: (entries) => entries.find(noError(contains(
                  { ePath: { unEntryPath: [`foo${n}`, `bar`]}
                  , eFields:
                    { a: { fValue: "[private]", fVisibility: "private" }
                    , c: { fValue: "d",         fVisibility: "public" }
                    }
                  })))
                }
              }
            }
          }
        ),
        done,
      })
    })

    it('finds a field', function (done) {
      let timestamp4 = Date(Date.now())
      GET({
        // dump: true,
        opts,
        url: `http://localhost:8081/api/v1/content/find?path=/foo${n}/bar&text=a&filter-field=a:value~z`,
        onSuccess: contains(
          { data:
            { dSubdirs:
              { [`foo${n}`]:
                { dEntries: (entries) => entries.find(noError(contains(
                  { ePath: { unEntryPath: [`foo${n}`, `bar`]}
                  , eFields:
                    { a: { fValue: "[private]", fVisibility: "private" }
                    , c: { fValue: "d",         fVisibility: "public" }
                    }
                  })))
                }
              }
            }
          }
        ),
        onError: r => explode(r),
        done,
      })
    })
  })

  describe("/delete", function () {
    it('deletes a field', function (done) {
      let timestamp4 = Date(Date.now())
      DELETE({
        // dump: true,
        opts,
        url: `http://localhost:8081/api/v1/content/delete-field?path=/foo${n}/bar&field=c`,
        onSuccess: contains(
          { data:
            { contents:
              { ePath: { unEntryPath: [`foo${n}`, "bar"] } // ???
              , eFields:
                { a: { fValue: "z", fVisibility: "private" }
                }
              , eTags: (tags) => json_equal(tags, [])
              , eDateModified: (date) => Date(date) >= timestamp4
              },
            }
          }),
        done,
      })
    })
  })
});
