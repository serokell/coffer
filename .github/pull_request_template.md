## Description

<!--
Describes the nature of your changes. If they are substantial, you should
further subdivide this into a section describing the problem you are solving and
another describing your solution.
-->

## Related issue(s)

<!--
Short description of how the PR relates to the issue, including an issue link.
For example:

- Fixed #100500 by adding lenses to exported items

Write 'None' if there are no related issues (which is discouraged).

If this PR does not fully resolve the linked issue and is not meant to close it,
replace `Fixed #` with `Fixed part of #`.
-->

Fixed #

## :white_check_mark: Checklist for your Pull Request

<!--
Ideally a PR has all of the checkmarks set.

If something in this list is irrelevant to your PR, you should still set this
checkmark indicating that you are sure it is dealt with (be that by irrelevance).

If you don't set a checkmark (e. g. don't add a test for new functionality),
you must be able to justify that.
-->

#### Related changes (conditional)

- Tests
  - [ ] If I added new functionality, I added tests covering it.
  - [ ] If I fixed a bug, I added a regression test to prevent the bug from
        silently reappearing again.

- Documentation
  - [ ] I checked whether I should update the docs and did so if necessary:
    - [README](../tree/master/README.md)
    - Haddock

<!-- TODO: uncomment this after the first release. -->
<!--
- Public contracts
  - [ ] Any modifications of public contracts comply with the [Evolution
  of Public Contracts](https://www.notion.so/serokell/Evolution-of-Public-Contracts-2a3bf7971abe4806a24f63c84e7076c5) policy.
  - [ ] I added an entry to the [changelog](../tree/master/CHANGES.md) if my changes are visible to the users
        and
  - [ ] provided a migration guide for breaking changes if possible
-->

#### Stylistic guide (mandatory)

- [ ] My commits comply with [the policy used in Serokell](https://www.notion.so/serokell/Where-and-how-to-commit-your-work-58f8973a4b3142c8abbd2e6fd5b3a08e).
- [ ] My code complies with the [style guide](../tree/master/docs/code-style.md).
