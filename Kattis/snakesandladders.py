import random


def main():
    initialize()
    snakes, ladders = read_board_layout_from_input()
    player1, player2 = read_player_names_from_input()
    winner = play_game(snakes, ladders, player1, player2)
    declare_winner(winner)


def initialize() -> None:
    random.seed(1337)


def read_board_layout_from_input():
    snakes, ladders = [None] * 101, [None] * 101
    for _ in range(int(input())):
        s, e = (int(i) for i in input().split())
        if s > e:
            snakes[s] = e
        else:
            ladders[s] = e
    return snakes, ladders


def read_player_names_from_input():
    return input(), input()


def play_game(snakes, ladders, player1, player2):
    pos1, pos2 = 0, 0
    while True:
        while True:
            d = roll_die()
            pos1 = min(100, pos1 + d)
            print(f"{player1} rolled {d} and is now at square {pos1}.")
            if pos1 >= 100:
                return player1
            if snakes[pos1] is not None:
                pos1 = snakes[pos1]
                print(f"Darn! A snake brought {player1} down to square {pos1}.")
            elif ladders[pos1] is not None:
                pos1 = ladders[pos1]
                print(f"Splendid! {player1} climbed a ladder up to square {pos1}.")
            if d != 6:
                break
        player1, player2, pos1, pos2 = player2, player1, pos2, pos1


def declare_winner(winner):
    print(f"{winner} won the game.")


def roll_die() -> int:
    return random.randint(1, 6)


if __name__ == "__main__":
    main()
