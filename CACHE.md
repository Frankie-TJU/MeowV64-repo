# 缓存设计

一致性状态：MSI

M：Modified，只有一个副本，并且是 dirty

S：Shared，可以有若干个副本，数据是 non-dirty

## L2

L2 给每个 DCache 提供了一个 Port，区分了 L1 发出的请求和 L2 发出的请求。

L1 可以发的请求有：

1. l1req = read(1)，读取的数据可以从 l2data 上得到。
2. l1req = modify(2)，读取的数据在 l2data 上，状态从 I/S 转移到 M，如果有 M 的缓存，L2 会发送 l2req = flush 和 l2req = invalidate；否则 L2 会向所有 S 状态的 cache 发送 l2req = invalidate
3. l1req = writeback(3)，写入的数据在 l1data 上，状态从 M 转移到 S

L2 可以发的请求有：

1. l2req = flush(1)，让 L1 进行写回操作，此时 l1data 为 L1 中缓存的 dirty 数据，状态 M -> S
2. l2req = invalidate(2)，让 L1 把 cache line 设为 invalid，状态 S -> I

### read

核心读取 L1 的时候，如果出现了 miss，首先要选取 victim，发送 l1req.writeback，把 victim 写回。

L1 发起 read 的时候，L1 一定处在 I 状态。L2 会检查是否 hit，如果也没找到，就从 AXI 上请求。

如果 L1 发起的 read，在 L2 上 hit 了，说明已经有核心有数据。如果有 L1 缓存处于 M 态，则 L2 发送 l2req.flush，L1 回应 l1req.writeback，L2 完成 I->S 和 M->S 的转移。

### write

核心写入 L1 的时候，如果出现了 miss，首先要选取 victim，发送 l1req.writeback，把 victim 写回。

L1 发起 write 的时候，发送 l1req.modify 到 L2。

## TileLink

L1 发请求：

1. read/modify，在 A channel 上发送 AcquireBlock（read 是 toB，modify 是 toT），在 D channel 上会收到 GrantData，这时候可以返回数据，并且在 E channel 上发送 GrantAck
2. writeback，在 C channel 上发送 ReleaseData，在 D channel 上会收到 ReleaseAck

L2 发请求：

1. 在 B channel 上收到 Probe，转为 l2req=flush 或者 invalidate，在 C channel 上发送 ProbeAck/ProbeAckData

分 channel：

A channel：负责处理 l1req，发送 AcquireBlock

B channel: 收到 Probe 的时候，发送 l2req

C channel: Arbiter：1) 负责处理 l1req，发送 Release Data 2) 发送 ProbeAck

D channel: 收到 ReleaseAck/GrantData 的时候，处理状态机

E channel: 负责发送 GrantAck