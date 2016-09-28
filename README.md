# mangopay api - erlang

The library `mangopay` is a wrapper for the MangoPay API written in Erlang.

## Installation

You can install `mangopay` via rebar3. Just add it as dependency to your `rebar.config`:

```erlang
{deps, [
  {mangopay, ".*", {git, "git://github.com/overture8/mangopay_erl.git", {branch, "master"}}}
]}.
```

Afterwards you can download and compile it:

```
rebar3 compile
```

## Usage

### Application

Even though it's a library, it has a state to keep. Thus, you must start its
application before calling any of the library's functions:

```erlang
application:start(mangopay).
```

### Authentication

At the moment, `mangopay` only supports authentication using OAuth2 Token (sent in
a header). To set it, run: 

```erlang
mangopay:auth("ClientId", "ClientPassword").
```

### API

#### Users

```erlang
mangopay:list_users().
```

## Implementation state

These are the priorities for me right now. If you want anything else feel free to add a pull request.

- [ ] Users
  - [ ] Create natural user
  - [ ] Update natural user
  - [ ] Create legal user
  - [ ] Update legal user
  - [ ] View user
  - [x] List users
- [ ] Wallets
  - [ ] Create wallet
  - [ ] Update wallet
  - [ ] View wallet
  - [ ] List wallet
- [ ] PayIns
 	- [ ] Create card direct PayIn
- [ ] PayOuts
 	- [ ] Create PayOut
 	- [ ] View PayOut
- [ ] BankAccounts
 	- [ ] Create GB bank account
- [ ] Cards
 	- [ ] Create card registration
 	- [ ] View card
- [ ] PreAuthorization
 	- [ ] Create pre-authorization
	

## License

The erlang mangopay lib is released under the [MIT License](http://www.opensource.org/licenses/MIT).
