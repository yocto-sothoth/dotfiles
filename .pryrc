def Pry.set_color(*sym, color)
  if sym.size == 1
    CodeRay::Encoders::Terminal::TOKEN_COLORS[sym[0]] = color
  else
    CodeRay::Encoders::Terminal::TOKEN_COLORS[sym[0]][sym[1]] = color
  end
end

Pry.config.color = true
Pry.set_color(:integer, "\e[0m")
Pry.set_color(:float, "\e[0m")
Pry.set_color(:method, "\e[1m")
Pry.set_color(:instance_variable, "\e[1m")
Pry.set_color(:string, :self, "\e[32m")
Pry.set_color(:string, :delimiter, "\e[32m")
Pry.set_color(:operator, "\e[32m")
Pry.set_color(:keyword, "\e[34m")

Pry::Commands.command(/^$/, 'repeat last command') { pry_instance.run_command(Pry.history.to_a.last) }
