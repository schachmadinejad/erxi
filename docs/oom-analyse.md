# OOM Analysis

This document summarizes memory risks in large inputs and how erxi mitigates them.

## Primary risk: String Table growth

The String Table stores values in global and local partitions. With unbounded
partitions, memory grows with the number of unique values.

Mitigations:
- `--value-max-length` limits which values are stored.
- `--value-capacity` bounds partition size and enables eviction.
- `--no-memory-monitor` disables warnings (not recommended for large inputs).

## Monitoring

`MemoryMonitor` tracks RSS and warns at 50/75/90% of system RAM; it aborts at 95%
to avoid swap thrashing.

## Large-file strategy

- Encode uses streaming (no full XML buffer in memory).
- Decode uses mmap for file inputs (feature `mmap`), avoiding full reads.

## Recommendations

For multi-GB XML with many unique values:
- Use `--value-max-length` and/or `--value-capacity`.
- Consider `--compression` only if CPU is available; compression requires buffering.
