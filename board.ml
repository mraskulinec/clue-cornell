open ANSITerminal

(** [print_location_board r] prints the current user's board based on
    which room r they are in *)
let print_location_board r =
  match r with
  | "the hall" ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "    \
         |-----------------------------------------------------------------|\n\
        \    |                |            |          |         \
         |              |\n\
        \    |    COCKTAIL    |            |   HALL   |         |  \
         DUFFIELD    |\n\
        \    |     LOUNGE     |            |     Ϙ    |         \
         |              |\n\
        \    |-----------------            |          |          \
         --------------|\n\
        \    |                             \
         |----------|                        |\n\
        \    \
         |---------------|_                                                \
         |\n\
        \    |  JANSEN'S       \
         |                                               |\n\
        \    |   DINING        |                           \
         |-------------------| \n\
        \    |    ROOM         |         ~~~~~~~~~~~~~~    \
         |                   | \n\
        \    |  AT BETHE      _|          CORNELL CLUE     \
         |                   | \n\
        \    |---------------|               BOARD         |    LAW \
         LIBRARY    | \n\
        \    |                           ~~~~~~~~~~~~~~    \
         |                   | \n\
        \    |-----------------|                           \
         |                   | \n\
        \    |                 |                           \
         |                   |   \n\
        \    |                 |                           \
         |---|               |\n\
        \    |    TERRACE      |                               \
         |---------------|\n\
        \    |                 \
         |                                               |\n\
        \    \
         |-----------------|                                               \
         |\n\
        \    |                        |----------------------|      \
         |----------|\n\
        \    |-----------------|      |                      |      |  \
         HELEN   |   \n\
        \    |                 |_     |       LIBE SLOPE     |      |  \
         NEWMAN  |\n\
        \    |    BAILEY HALL    |    |                      |      \
         |          |\n\
        \    |                   |    |--|                |--|      \
         |          | \n\
        \    \
         |-----------------------------------------------------------------|\n\
        \ Ϙ = You are in The Hall!\n"
  | "jansen's dining" ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "|-----------------------------------------------------------------|\n\
        \        |                |            |          |         \
         |              |\n\
        \        |    COCKTAIL    |            |   HALL   |         |  \
         DUFFIELD    |\n\
        \        |     LOUNGE     |            |          |         \
         |              |\n\
        \        |-----------------            |          |          \
         --------------|\n\
        \        |                             \
         |----------|                        |\n\
        \        \
         |---------------|_                                                \
         |\n\
        \        |  JANSEN'S       \
         |                                               |\n\
        \        |   DINING        |                           \
         |-------------------| \n\
        \        |    ROOM    փ    |         ~~~~~~~~~~~~~~   \
         |                   | \n\
        \        |  AT BETHE      _|          CORNELL CLUE     \
         |                   | \n\
        \        |---------------|               BOARD         |    \
         LAW LIBRARY    | \n\
        \        |                           ~~~~~~~~~~~~~~    \
         |                   | \n\
        \        |-----------------|                           \
         |                   | \n\
        \        |                 |                           \
         |                   |   \n\
        \        |                 |                           \
         |---|               |\n\
        \        |    TERRACE      |                               \
         |---------------|\n\
        \        |                 \
         |                                               |\n\
        \        \
         |-----------------|                                               \
         |\n\
        \        |                        \
         |----------------------|      |----------|\n\
        \        |-----------------|      |                      \
         |      |  HELEN   |   \n\
        \        |                 |_     |       LIBE SLOPE     \
         |      |  NEWMAN  |\n\
        \        |    BAILEY HALL    |    |                      \
         |      |          |\n\
        \        |                   |    |--|                \
         |--|      |          | \n\
        \        \
         |-----------------------------------------------------------------|   \n\
        \        \n\
        \         փ = You are in Jansen's Dining Room!\n"
  | "law library" ->
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "|-----------------------------------------------------------------|\n\
        \      |                |            |          |         \
         |              |\n\
        \      |    COCKTAIL    |            |   HALL   |         |  \
         DUFFIELD    |\n\
        \      |     LOUNGE     |            |          |         \
         |              |\n\
        \      |-----------------            |          |          \
         --------------|\n\
        \      |                             \
         |----------|                        |\n\
        \      \
         |---------------|_                                                \
         |\n\
        \      |  JANSEN'S       \
         |                                               |\n\
        \      |   DINING        |                           \
         |-------------------| \n\
        \      |    ROOM         |         ~~~~~~~~~~~~~~    \
         |                   | \n\
        \      |  AT BETHE      _|          CORNELL CLUE     \
         |                   | \n\
        \      |---------------|               BOARD         |    LAW \
         LIBRARY    | \n\
        \      |                           ~~~~~~~~~~~~~~    \
         |                   | \n\
        \      |-----------------|                           |         \
         ۩         | \n\
        \      |                 |                           \
         |                   |   \n\
        \      |                 |                           \
         |---|               |\n\
        \      |    TERRACE      |                               \
         |---------------|\n\
        \      |                 \
         |                                               |\n\
        \      \
         |-----------------|                                               \
         |\n\
        \      |                        |----------------------|      \
         |----------|\n\
        \      |-----------------|      |                      |      \
         |  HELEN   |   \n\
        \      |                 |_     |       LIBE SLOPE     |      \
         |  NEWMAN  |\n\
        \      |    BAILEY HALL    |    |                      |      \
         |          |\n\
        \      |                   |    |--|                |--|      \
         |          | \n\
        \      \
         |-----------------------------------------------------------------| \n\
        \      \n\
        \ ۩ = You are in the law library!\n"
  | "cocktail lounge" ->
      ANSITerminal.print_string
        [ ANSITerminal.magenta ]
        "|-----------------------------------------------------------------|\n\
        \      |       △        |            |          |         \
         |              |\n\
        \      |    COCKTAIL    |            |   HALL   |         |  \
         DUFFIELD    |\n\
        \      |     LOUNGE     |            |          |         \
         |              |\n\
        \      |-----------------            |          |          \
         --------------|\n\
        \      |                             \
         |----------|                        |\n\
        \      \
         |---------------|_                                                \
         |\n\
        \      |  JANSEN'S       \
         |                                               |\n\
        \      |   DINING        |                           \
         |-------------------| \n\
        \      |    ROOM         |         ~~~~~~~~~~~~~~    \
         |                   | \n\
        \      |  AT BETHE      _|          CORNELL CLUE     \
         |                   | \n\
        \      |---------------|               BOARD         |    LAW \
         LIBRARY    | \n\
        \      |                           ~~~~~~~~~~~~~~    \
         |                   | \n\
        \      |-----------------|                           \
         |                   | \n\
        \      |                 |                           \
         |                   |   \n\
        \      |                 |                           \
         |---|               |\n\
        \      |    TERRACE      |                               \
         |---------------|\n\
        \      |                 \
         |                                               |\n\
        \      \
         |-----------------|                                               \
         |\n\
        \      |                        |----------------------|      \
         |----------|\n\
        \      |-----------------|      |                      |      \
         |  HELEN   |   \n\
        \      |                 |_     |       LIBE SLOPE     |      \
         |  NEWMAN  |\n\
        \      |    BAILEY HALL    |    |                      |      \
         |          |\n\
        \      |                   |    |--|                |--|      \
         |          | \n\
        \      \
         |-----------------------------------------------------------------| \n\
        \        \n\
        \ △ = You are in the cocktail lounge!\n"
  | "terrace restaurant" ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "|-----------------------------------------------------------------|\n\
        \      |                |            |          |         \
         |              |\n\
        \      |    COCKTAIL    |            |   HALL   |         |  \
         DUFFIELD    |\n\
        \      |     LOUNGE     |            |          |         \
         |              |\n\
        \      |-----------------            |          |          \
         --------------|\n\
        \      |                             \
         |----------|                        |\n\
        \      \
         |---------------|_                                                \
         |\n\
        \      |  JANSEN'S       \
         |                                               |\n\
        \      |   DINING        |                           \
         |-------------------| \n\
        \      |    ROOM         |         ~~~~~~~~~~~~~~    \
         |                   | \n\
        \      |  AT BETHE      _|          CORNELL CLUE     \
         |                   | \n\
        \      |---------------|               BOARD         |    LAW \
         LIBRARY    | \n\
        \      |                           ~~~~~~~~~~~~~~    \
         |                   | \n\
        \      |-----------------|                           \
         |                   | \n\
        \      |       X         |                           \
         |                   |   \n\
        \      |                 |                           \
         |---|               |\n\
        \      |    TERRACE      |                               \
         |---------------|\n\
        \      |                 \
         |                                               |\n\
        \      \
         |-----------------|                                               \
         |\n\
        \      |                        |----------------------|      \
         |----------|\n\
        \      |-----------------|      |                      |      \
         |  HELEN   |   \n\
        \      |                 |_     |       LIBE SLOPE     |      \
         |  NEWMAN  |\n\
        \      |    BAILEY HALL    |    |                      |      \
         |          |\n\
        \      |                   |    |--|                |--|      \
         |          | \n\
        \      \
         |-----------------------------------------------------------------| \n\
        \      \n\
        \ X = You are at terrace restaurant!\n"
  | "bailey hall" ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "|-----------------------------------------------------------------|\n\
        \      |                |            |          |         \
         |              |\n\
        \      |    COCKTAIL    |            |   HALL   |         |  \
         DUFFIELD    |\n\
        \      |     LOUNGE     |            |          |         \
         |              |\n\
        \      |-----------------            |          |          \
         --------------|\n\
        \      |                             \
         |----------|                        |\n\
        \      \
         |---------------|_                                                \
         |\n\
        \      |  JANSEN'S       \
         |                                               |\n\
        \      |   DINING        |                           \
         |-------------------| \n\
        \      |    ROOM         |         ~~~~~~~~~~~~~~    \
         |                   | \n\
        \      |  AT BETHE      _|          CORNELL CLUE     \
         |                   | \n\
        \      |---------------|               BOARD         |    LAW \
         LIBRARY    | \n\
        \      |                           ~~~~~~~~~~~~~~    \
         |                   | \n\
        \      |-----------------|                           \
         |                   | \n\
        \      |                 |                           \
         |                   |   \n\
        \      |                 |                           \
         |---|               |\n\
        \      |    TERRACE      |                               \
         |---------------|\n\
        \      |                 \
         |                                               |\n\
        \      \
         |-----------------|                                               \
         |\n\
        \      |                        |----------------------|      \
         |----------|\n\
        \      |-----------------|      |                      |      \
         |  HELEN   |   \n\
        \      |                 |_     |       LIBE SLOPE     |      \
         |  NEWMAN  |\n\
        \      |    BAILEY HALL    |    |                      |      \
         |          |\n\
        \      |         ♫         |    |--|                \
         |--|      |          | \n\
        \      \
         |-----------------------------------------------------------------| \n\
        \ ♫ = You are in bailey hall!\n"
  | "libe slope" ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "|-----------------------------------------------------------------|\n\
        \      |                |            |          |         \
         |              |\n\
        \      |    COCKTAIL    |            |   HALL   |         |  \
         DUFFIELD    |\n\
        \      |     LOUNGE     |            |          |         \
         |              |\n\
        \      |-----------------            |          |          \
         --------------|\n\
        \      |                             \
         |----------|                        |\n\
        \      \
         |---------------|_                                                \
         |\n\
        \      |  JANSEN'S       \
         |                                               |\n\
        \      |   DINING        |                           \
         |-------------------| \n\
        \      |    ROOM         |         ~~~~~~~~~~~~~~    \
         |                   | \n\
        \      |  AT BETHE      _|          CORNELL CLUE     \
         |                   | \n\
        \      |---------------|               BOARD         |    LAW \
         LIBRARY    | \n\
        \      |                           ~~~~~~~~~~~~~~    \
         |                   | \n\
        \      |-----------------|                           \
         |                   | \n\
        \      |                 |                           \
         |                   |   \n\
        \      |                 |                           \
         |---|               |\n\
        \      |    TERRACE      |                               \
         |---------------|\n\
        \      |                 \
         |                                               |\n\
        \      \
         |-----------------|                                               \
         |\n\
        \      |                        |----------------------|      \
         |----------|\n\
        \      |-----------------|      |                      |      \
         |  HELEN   |   \n\
        \      |                 |_     |       LIBE SLOPE     |      \
         |  NEWMAN  |\n\
        \      |    BAILEY HALL    |    |          ♛           \
         |      |          |\n\
        \      |                   |    |--|                |--|      \
         |          | \n\
        \      \
         |-----------------------------------------------------------------| \n\
        \ ♛ = You are on libe slope!\n"
  | "helen newman" ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "|-----------------------------------------------------------------|\n\
        \      |                |            |          |         \
         |              |\n\
        \      |    COCKTAIL    |            |   HALL   |         |  \
         DUFFIELD    |\n\
        \      |     LOUNGE     |            |          |         \
         |              |\n\
        \      |-----------------            |          |          \
         --------------|\n\
        \      |                             \
         |----------|                        |\n\
        \      \
         |---------------|_                                                \
         |\n\
        \      |  JANSEN'S       \
         |                                               |\n\
        \      |   DINING        |                           \
         |-------------------| \n\
        \      |    ROOM         |         ~~~~~~~~~~~~~~    \
         |                   | \n\
        \      |  AT BETHE      _|          CORNELL CLUE     \
         |                   | \n\
        \      |---------------|               BOARD         |    LAW \
         LIBRARY    | \n\
        \      |                           ~~~~~~~~~~~~~~    \
         |                   | \n\
        \      |-----------------|                           \
         |                   | \n\
        \      |                 |                           \
         |                   |   \n\
        \      |                 |                           \
         |---|               |\n\
        \      |    TERRACE      |                               \
         |---------------|\n\
        \      |                 \
         |                                               |\n\
        \      \
         |-----------------|                                               \
         |\n\
        \      |                        |----------------------|      \
         |----------|\n\
        \      |-----------------|      |                      |      \
         |  HELEN   |   \n\
        \      |                 |_     |       LIBE SLOPE     |      \
         |  NEWMAN  |\n\
        \      |    BAILEY HALL    |    |                      |      \
         |     ✱    |\n\
        \      |                   |    |--|                |--|      \
         |          | \n\
        \      \
         |-----------------------------------------------------------------| \n\
        \ ✱ = You are in helen newman!\n"
  | "duffield hall" ->
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "|-----------------------------------------------------------------|\n\
        \      |                |            |          |         \
         |              |\n\
        \      |    COCKTAIL    |            |   HALL   |         |  \
         DUFFIELD    |\n\
        \      |     LOUNGE     |            |          |         \
         |      ★       |\n\
        \      |-----------------            |          |          \
         --------------|\n\
        \      |                             \
         |----------|                        |\n\
        \      \
         |---------------|_                                                \
         |\n\
        \      |  JANSEN'S       \
         |                                               |\n\
        \      |   DINING        |                           \
         |-------------------| \n\
        \      |    ROOM         |         ~~~~~~~~~~~~~~    \
         |                   | \n\
        \      |  AT BETHE      _|          CORNELL CLUE     \
         |                   | \n\
        \      |---------------|               BOARD         |    LAW \
         LIBRARY    | \n\
        \      |                           ~~~~~~~~~~~~~~    \
         |                   | \n\
        \      |-----------------|                           \
         |                   | \n\
        \      |                 |                           \
         |                   |   \n\
        \      |                 |                           \
         |---|               |\n\
        \      |    TERRACE      |                               \
         |---------------|\n\
        \      |                 \
         |                                               |\n\
        \      \
         |-----------------|                                               \
         |\n\
        \      |                        |----------------------|      \
         |----------|\n\
        \      |-----------------|      |                      |      \
         |  HELEN   |   \n\
        \      |                 |_     |       LIBE SLOPE     |      \
         |  NEWMAN  |\n\
        \      |    BAILEY HALL    |    |                      |      \
         |          |\n\
        \      |                   |    |--|                |--|      \
         |          | \n\
        \      \
         |-----------------------------------------------------------------| \n\
        \ ★ = You are in duffield hall!\n"
  | _ -> failwith "unimplemented board"