-module(road).
-compile(export_all).

main() ->
  File = "road.txt",
  {ok, Bin} = file:read_file(File),
  parse_map(bin)