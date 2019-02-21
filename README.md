# opennode-haskell

I. Introduction
---------------

A Haskell client for the opennode API (https://www.opennode.co/). You would need an account with opennode in order to generate an API token for authentication. So far the operations implemented are:

- Fetch all pending and confirmed withdrawals (`/v1/withdrawals`)
- Fetch information about a specific withdrawal (`/v1/withdrawal/{id}`)
- Fetch all paid and processing charges (`/v1/charges`)
- Fetch information about a specific charge (`/v1/charge/{id}`)

II. Build
----------
The project is using `stack` for building the project and managing dependencies (https://docs.haskellstack.org/en/stable/README/). 

To build the project `cd` on the project directory and run:

```
$ stack build
```

III. Notes
-------------------

- This is a work in progress.
- The tokens found on the git history are revoked.
