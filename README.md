# opennode-haskell

I. Introduction
---------------

A Haskell client for the opennode API (https://www.opennode.com/). You would need an account with opennode in order to generate an API token for authentication. So far the operations implemented are:

- Get all pending and confirmed withdrawals (`GET` `/v1/withdrawals`)
- Get information about a specific withdrawal (`GET` `/v1/withdrawal/{id}`)
- Initiate a withdrawal (`POST` `/v2/withdrawals`)

- Get all paid and processing charges (`GET` `/v1/charges`)
- Get information about a specific charge (`GET` `/v1/charge/{id}`)
- Generate a charge (`POST` `v1/charges`)

- Create a refund (`POST` `v1/refunds`)
- List all refunds (`GET` `/v1/refunds`)
- Get information about a specific refund (`GET` `/v1/refund/{id}`)

- Get the balance of an account (`GET` `/v1/account/balance`)
- Get the exchange rates (`GET` `/v1/rates`)
- Get all supported currencies (`GET` `/v1/currencies`)

II. Build
----------
The project is using `stack` for building the project and managing dependencies (https://docs.haskellstack.org/en/stable/README/). 

To build the project `cd` on the project directory and run:

```
$ stack build --fast
```
