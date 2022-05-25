<!--
 - SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -->

# WEB API Error Codes

These codes are grouped in the following way:
- `view` errors: 100-102
- `create` errors: 200-202
- `set-field` errors: 300-301
- `delete-field` errors: 400-401
- `rename`/`copy` errors: 500-505
- `delete` errors: 600-601
- `tag` errors: 700-702

For internal server errors we have special zero (0) code.

See the following tables for detailed explanations of error codes.

## View
| Error                    | Code |
|:-------------------------|:----:|
| Path not found           | 100  |
| Directory no field match | 101  |
| Entry no field match     | 102  |

## Create
| Error                        | Code |
|:-----------------------------|:----:|
| Parent directory is an entry | 200  |
| Destination is directory     | 201  |
| Entry already exists         | 202  |

## Set-field
| Error                  | Code |
|:-----------------------|:----:|
| Entry not found        | 300  |
| Missing field contents | 301  |

## Delete-field
| Error           | Code |
|:----------------|:----:|
| Entry not found | 400  |
| Field not found | 401  |

## Rename/Copy
| Error                        | Code |
|:-----------------------------|:----:|
| Path not found               | 500  |
| Missing entry name           | 501  |
| Same path                    | 502  |
| Parent directory is an entry | 503  |
| Destination is directory     | 504  |
| Entry already exists         | 505  |

## Delete
| Error           | Code |
|:----------------|:----:|
| Path not found  | 600  |
| Directory found | 601  |

## Tag
| Error           | Code |
|:----------------|:----:|
| Entry not found | 700  |
| Tag not found   | 701  |
| Duplicate tag   | 702  |
