---
layout: post
title: An Introduction to the Raft Consensus Protocol
excerpt: Consensus doesn't always mean Paxos.
categories: articles
tags: [consensus,raft,protocol,distributed,computing]
comments: true
share: true
ads: true
---

Raft is a [consensus protocol](https://en.wikipedia.org/wiki/Consensus_(computer_science)). It is touted as being an understandable alternative to the popular [Paxos algorithm](https://en.wikipedia.org/wiki/Paxos_(computer_science)). While I find this to be true, it appears that there are few relevant brief overview explanations existing on the Internet at this point. Though it is a reasonably new protocol, [the original paper](http://ramcloud.stanford.edu/raft.pdf) is the source of information I have been able to find about this protocol that explains in words how this operates. That being said, there is a [nice visual walktrhough of the protocol](http://thesecretlivesofdata.com/raft/), it's difficult to use this as a reference which you can quickly skim. Additionally, what appears to be [the official Raft protocol homepage](https://raft.github.io) has only paper references and reference implementations. As a result, this post is meant to **augment** these existing resources and not to replace them (read: the information contained is largely the same).

With proper reference implementations, perhaps people have found it sufficient to ignore the details of their underlying network and allow "magic" to happen while the network guys worry about hammering out the details. While I'm sure many take this approach, it's still quite worthwhile to understand the elements of your system; and so here we are.

> *NOTE:* This post is intended to be a tl;dr for the paper. That being said, I highly recommend reviewing the original source as it covers many details and corner cases which will be omitted here (the proofs are particularly useful).

# Summary

## Stages

The Raft protocol decomposes into **3** distinct stages; each stage is a logical decomposition of the authors' solution to the consensus problem.

  1. **Leader election.** This is the first step to occur and it occurs every time a leader fails (i.e. reelection). The elected leader is solely responsible for the replicated log to be shared among nodes.
  1. **Log replication.** The leader accepts client requests and copies them to each node in the cluster.
  1. **Safety.** Due to the following rules, we ensure that the logs remain consistent across the cluster:
    * *At most* a single (1) leader is elected at any given time.
    * Leaders can only append to logs. Modifications or deletions from the log are *explicitly* forbidden.
    * If a particular log entry *E* is *committed*, then *E* will be present in the logs of all leaders (current and future).
    * For some log entry *E* at index *i*, if *E* is applied to a state machine, then no other server will apply an entry other than *E* for index *i*.

## Node states

Individually, nodes in a cluster can be in one of **3** states:

  1. **Leader.** The node is the leader of the cluster. The leader receives requests from clients and replicates them over the cluster.
  1. **Follower.** The node is explicitly *not* a leader in the active cluster. They are passive workers in the cluster.
  1. **Candidate.** The node is a potential leader during an election round. If the candidate wins the election, it will become the new leader. Otherwise, it becomes a follower.

## Node communication

Finally, there are **2** major RPCs which are used to carry out the protocol operations:

  1. **AppendEntries.** Invoked by the leader for log replication. Additionally, this RPC is used as a heartbeat.
  1. **RequestVote.** Invoked by the candidates for gathering votes during a leader election. Based on particular criteria, a leader will be chosen at the end of the process.

# A more in-depth look

Each node holds a bit of state. The state includes a log containing recorded entries, a currentTerm representing the latest term (i.e. protocol round/vaguely number of elections) this node has seen, a commitIndex holding the highest log index which has been committed, and other items [which are discussed in the paper](http://ramcloud.stanford.edu/raft.pdf) but omitted here. This state makes up for the foundation of the protocol's behavior.

## Leader election

Leader elections occur when the current leader is detected to have timed out (i.e. no heartbeats received within an *election timeout*). As a result, when the system is initialized, all nodes begin in the *follower* state. A node remains in this state while it continues to receive valid heartbeats from the leader. However, when the leader times out, the node begins an election with the remaining nodes.

> NOTE: *election timeout* is typically a number chosen at random between some [min, max] interval.

To start an election, the node increments its current term and transitions to the *candidate* state. Additionally, it votes for itself and issues a RequestVote RPC to all other nodes in the cluster. If a node receives an AppendEntries RPC from another node while in the *candidate* state, it moves to the *follower* state if and only if the requestor's current term is greater than or equal to the node's current term. Otherwise, the RPC is rejected. On the other hand, a *candidate* node may transition to the *leader* state if it receives a **majority** (i.e. >50%) of the votes from the cluster.

Another possibility is a draw (i.e. no consensus is reached); this occurs when the *election timeout* expires during the election process. If this is the case, a new *election timeout* is randomly selected in the predetermined interval and a new election is started in the same fashion as before.

## Log replication

After a leader has been selected, it can serve client requests. For each request, the leader appends its log with a new entry and issues an AppendEntries RPC in parallel to all nodes in the cluster. After the request has been successfully replicated, the leader *commits* the log entry (i.e. applies it to state machine/executes the command) and returns the response directly to the client.

The major take-away here is that information flows in a single direction: from leader to follower. Followers never update their own state unless the leader has directed them to do so. Consequently, many important properties can be proved about this system. However, those details are omitted here.

# Membership Changes

Until now, we've only discussed the protocol at it's most fundamental level. That is, we've discussed how to re-elect leaders upon failures and how to replicate requests, but we have not yet laid out the foundation for general membership changes in the system. This is intentional. Since the authors of the original paper omit this until the end, I thought I would perpetuate this in the same spirit as an attempt to keep things clear and understandable.

To modify the cluster configuration without bringing it down (i.e. elastically increasing or decreasing the number of nodes in the cluster), the authors present a two-phase approach to ensuring safety of the system. They first reach a state known as *joint consensus* which combines the old and new configurations. After this phase has been committed, the new configuration takes charge of the cluster.

# Log Compaction

The final practical element to Raft that should be considered is log compaction. Log compaction is a method to reduce the size of the logs which would otherwise go without bound; if this happened in practice your machine would run out of memory and all would be lost.

The approach suggested in the paper is *snapshotting*. Snapshotting is the task of copying the entire existing system state and comitting it as a log entry. At this point, all remaining log entries can be safely discarded.

To avoid excessive overhead which may be associated with snapshotting, it is best to amortize this cost by fixing an appropriate size for the node's log. When the node reaches this predefined capacity, snapshotting should occur.

# Conclusion

Raft is a conceptually clear solution to the consensus problem in distributed systems. It provides an intuitive structure and practical basis for designing *real* distributed systems. The original Raft paper by Ongaro and Ousterhout is incredibly accessible by anyone's standards; this is even more true since it's an academic paper. As a result, [I encourage you to read it](http://ramcloud.stanford.edu/raft.pdf) for deeper detail we may have skipped over here.
