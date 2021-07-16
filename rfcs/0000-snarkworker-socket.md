## Summary
[summary]: #summary

Pools of Snarkworkers can be made more robust and be load balanced by moving to a persistant socket connection for work requests and responses.

## Terms

snark-coordinator (SC) - daemon(s) participating on the gossip network and queueing up snark-work to be done by snark-workers

snark-worker (SW) - daemon(s) processing compute intensive snarks (snark+libsnark)


## Motivation

[motivation]: #motivation


Current snark-worker (SW) to snark-coordinator (SC) communications are done with two discrete communication steps.
1. SW connect to SC on rpc port and request some work.
(SW does work.)
2. SW again connect to SC on rpc port and sends completed work.

If a SW requests work and then dies, the work is NOT re-assigned as the SC is just stuck waiting for the completed work.

SWs are also only configured to poll a single SC daemon.  They cannot service many SC daemons (eg. on different testnets) concurrently.

Using a persistant socket connection, we would be able to tell when a SW has died (socket will disconnect) and re-assign the work to another SW.  Using persistant network connections will also ensure that TCP load balancing can be used to connect one large SW to many independant SC daemons.


## Detailed design

[detailed-design]: #detailed-design

This is still up for debate and will depend on what the ocaml rpc lib supports and what's easiest to build.

Possible scenarios:

* SOCKET: SW round robins to multiple SC endpoints and maintains tcp connection until work is complete and sent
* Flip client/server: SC daemon is no longer polled, but instead SC pushes work OUT to a pool of SWs waiting for incoming requests.


## Drawbacks
[drawbacks]: #drawbacks

Maintaining a connection means extra state is kept between a SW daemon and a SC daemon.

If snarkwork becomes trivially easy (HW accelerated), there would be less need to load balance worker compute power.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

* Socketbased load balancing is well understood and well tested.
* Using something like HTTP Cookies could provide similar techniques, but require transport changes.
* Without doing this we risk losing snark work and getting in a incomplete state.

## Prior art
[prior-art]: #prior-art

NA

## Unresolved questions
[unresolved-questions]: #unresolved-questions

* Best way to accompish??
