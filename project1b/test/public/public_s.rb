require_relative "../../src/controllers/input_controller.rb"
require_relative "../../src/controllers/game_controller.rb"
require_relative "../../src/models/game_board.rb"
require_relative "../../src/models/position.rb"
require_relative "../../src/models/ship.rb"


gameboard = GameBoard.new(10,10)

ship1 = Ship.new(Position.new(2,2), "Right", 2)
ship2 = Ship.new(Position.new(9,5), "Up", 3)
ship3 = Ship.new(Position.new(6,9), "Left", 3)
ship4 = Ship.new(Position.new(7,2), "Down", 2)
ship5 = Ship.new(Position.new(7,3), "Down", 2)

gameboard.add_ship(ship1)
gameboard.add_ship(ship2)
gameboard.add_ship(ship3)
gameboard.add_ship(ship4)
gameboard.add_ship(ship5)

gameboard.to_s