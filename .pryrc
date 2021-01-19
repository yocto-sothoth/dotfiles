Pry::Commands.command(/^$/, 'repeat last command') { pry_instance.run_command(Pry.history.to_a.last) }
