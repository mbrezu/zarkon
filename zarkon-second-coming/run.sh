#!/bin/sh

rm *.fasl
rm log.txt
rm proxy-bot.log
rm -rf images
mkdir images
# make
cd ../aichallenge
MAP=maps/maze/maze_04p_01.map
MAP21=maps/random_walk/random_walk_02p_01.map
MAP22=maps/random_walk/random_walk_02p_02.map
MAP31=maps/random_walk/random_walk_03p_01.map
MAP32=maps/random_walk/random_walk_03p_02.map
MAP41=maps/random_walk/random_walk_04p_01.map
MAP42=maps/random_walk/random_walk_04p_02.map
MAP61=maps/random_walk/random_walk_06p_01.map
MAP62=maps/random_walk/random_walk_06p_02.map
MAP91=maps/random_walk/random_walk_09p_01.map
MAP92=maps/random_walk/random_walk_09p_02.map

MAZ21=maps/maze/maze_02p_01.map
MAZ22=maps/maze/maze_02p_02.map
MAZ31=maps/maze/maze_03p_01.map
MAZ41=maps/maze/maze_04p_01.map
MAZ42=maps/maze/maze_04p_02.map
MAZ51=maps/maze/maze_05p_01.map
MAZ61=maps/maze/maze_06p_01.map
MAZ71=maps/maze/maze_07p_01.map
MAZ81=maps/maze/maze_08p_01.map

MMAZ21=maps/multi_hill_maze/maze_02p_01.map
MMAZ22=maps/multi_hill_maze/maze_02p_02.map
MMAZ31=maps/multi_hill_maze/maze_03p_01.map
MMAZ41=maps/multi_hill_maze/maze_04p_01.map
MMAZ42=maps/multi_hill_maze/maze_04p_02.map
MMAZ51=maps/multi_hill_maze/maze_05p_01.map
MMAZ71=maps/multi_hill_maze/maze_07p_01.map
MMAZ81=maps/multi_hill_maze/maze_08p_01.map

CAP21=maps/cell_maze/cell_maze_p02_01.map
CAP22=maps/cell_maze/cell_maze_p02_02.map
CAP23=maps/cell_maze/cell_maze_p02_03.map
CAP31=maps/cell_maze/cell_maze_p03_01.map
CAP32=maps/cell_maze/cell_maze_p03_02.map
CAP33=maps/cell_maze/cell_maze_p03_03.map
CAP34=maps/cell_maze/cell_maze_p03_04.map
CAP41=maps/cell_maze/cell_maze_p04_01.map
CAP42=maps/cell_maze/cell_maze_p04_02.map
CAP43=maps/cell_maze/cell_maze_p04_03.map
CAP44=maps/cell_maze/cell_maze_p04_04.map
CAP51=maps/cell_maze/cell_maze_p05_01.map
CAP52=maps/cell_maze/cell_maze_p05_02.map
CAP53=maps/cell_maze/cell_maze_p05_03.map
CAP61=maps/cell_maze/cell_maze_p06_01.map
CAP62=maps/cell_maze/cell_maze_p06_02.map
CAP71=maps/cell_maze/cell_maze_p07_01.map
CAP72=maps/cell_maze/cell_maze_p07_02.map

./playgame.py -So --player_seed 42 \
    --engine_seed 42 \
    --end_wait=0.25 --verbose --log_dir game_logs --turns 1000 --map_file $MAP41 "$@" \
    "python sample_bots/python/HunterBot.py" \
    "python sample_bots/python/HunterBot.py" \
    "python sample_bots/python/HunterBot.py" \
    "../zarkon-second-coming/MyBot" \
| tee ../zarkon-second-coming/last-game.txt |
java -jar visualizer.jar

    # --end_wait=0.25 --verbose --log_dir game_logs --turns 1000 --map_file $MAP22 "$@" \
    # "../oldies/MyBot-22" \


# ./playgame.py -So --player_seed 42 \
#     --engine_seed 42 \
#     --end_wait=0.25 --verbose --log_dir game_logs --turns 1000 --map_file $MMAZ41 "$@" \
#     "python sample_bots/python/LeftyBot.py" \
#     "python sample_bots/python/LeftyBot.py" \
#     "python sample_bots/python/LeftyBot.py" \
#     "../zarkon-second-coming/MyBot" |
# tee ../zarkon-second-coming/last-game.txt |
# java -jar visualizer.jar

# ./playgame.py -So --player_seed 42 \
#     --engine_seed 42 \
#     --end_wait=0.25 --verbose --log_dir game_logs --turns 1000 --map_file $MAP41 "$@" \
#     "python sample_bots/python/HunterBot.py" \
#     "python sample_bots/python/HunterBot.py" \
#     "python sample_bots/python/HunterBot.py" \
#     "../zarkon-second-coming/MyBot" |
# tee ../zarkon-second-coming/last-game.txt |
# java -jar visualizer.jar
#(cd visualizer; python visualize_locally.py)


