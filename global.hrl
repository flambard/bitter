-define(VERSION_STRING, "0.0.00").
-define(COMPACT_VERSION_STRING, "0000").

-define(PORT, 6881). % Typically 6881-6889

-define(BLOCK_SIZE, 16384).

-define(TIMEOUT, 120000). % 2 minutes

-define(INITIAL_SOCKET_SETTINGS, [binary, {packet, raw}, {active, false}, {reuseaddr, true}]). %% Reuse during development..
