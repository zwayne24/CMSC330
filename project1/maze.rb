#!/usr/local/bin/ruby

# ########################################
# CMSC 330 - Project 1
# ########################################

#-----------------------------------------------------------
# FUNCTION DECLARATIONS
#-----------------------------------------------------------
def parse(file)
  puts "Not yet implemented"
end

#-----------------------------------------------------------
# the following is a parser that reads in a simpler version
# of the maze files.  Use it to get started writing the rest
# of the assignment.  You can feel free to move or modify
# this function however you like in working on your assignment.

def read_and_print_simple_file(file)
  line = file.gets
  if line == nil then return end


    # read 1st line, must be maze header
    @sz, @sx, @sy, @ex, @ey = line.split(/\s/)
    @sz = @sz.to_i
    #puts "header spec: size=#{sz}, start=(#{sx},#{sy}), end=(#{ex},#{ey})"

    # read additional lines
    while line = file.gets do

      # begins with "path", must be path specification
      if line[0...4] == "path"
        p, name, x, y, ds = line.split(/\s/)
        @path[name]=[x,y,ds]
        #puts "path spec: #{name} starts at (#{x},#{y}) with dirs #{ds}"

        # otherwise must be cell specification (since maze spec must be valid)
      else
        x, y, ds, w = line.split(/\s/,4)
        @a[[x.to_i,y.to_i]] = [ds,w]
        # puts "cell spec: coordinates (#{x},#{y}) with dirs #{ds}"
        #ws = w.split(/\s/)
        #ws.each {|w| puts "  weight #{w}"}
      end
    end
  end

  def opens(file)

    count = 0
    x=0
    y=0
    while y < @sz.to_i do
      while x < @sz.to_i do
        if @a[[x,y]][0] =~ /[lrud][lrud][lrud][lrud]/
          count+=1
        end
        x+=1
      end
      y+=1
      x=0
    end
    return count
  end

  def bridge(file)

    y = 0
    x = 0
    count = 0
    @sz = @sz.to_i
    while y < @sz do
      while x < @sz-1 do
        if @a[[x,y]][0].include?("r") && @a[[x+1,y]][0].include?("r")
          count+=1
        end
        x+=1
      end
      x = 0
      y+=1
    end

    x = 0
    y = 0

    while x < @sz do
      while y < @sz-1 do
        if @a[[x,y]][0].include?("d") && @a[[x,y+1]][0].include?("d")
          count+=1
        end
        y+=1
      end
      y = 0
      x+=1
    end

    return count
  end

  def sortcells(file)

    x = 0
    y = 0
    ret = Array.new

    ret.push("0")
    @sz = @sz.to_i
    while x < @sz do
      while y < @sz do
        if @a[[x,y]][0].length == 0
          ret[0]+=",(#{x},#{y})"
        end
        y+=1
      end
      y = 0
      x+=1
    end

    x = 0
    y = 0

    ret.push("1")
    @sz = @sz.to_i
    while x < @sz do
      while y < @sz do
        if  @a[[x,y]][0].length == 1
          ret[1]+=",(#{x},#{y})"
        end
        y+=1
      end
      y = 0
      x+=1
    end

    x = 0
    y = 0

    ret.push("2")
    @sz = @sz.to_i
    while x < @sz do
      while y < @sz do
        if @a[[x,y]][0].length == 2
          ret[2]+=",(#{x},#{y})"
        end
        y+=1
      end
      y = 0
      x+=1
    end

    x = 0
    y = 0

    ret.push("3")
    @sz = @sz.to_i
    while x < @sz do
      while y < @sz do
        if @a[[x,y]][0].length == 3
          ret[3]+=",(#{x},#{y})"
        end
        y+=1
      end
      y = 0
      x+=1
    end

    x = 0
    y = 0

    ret.push("4")
    @sz = @sz.to_i
    while x < @sz do
      while y < @sz do
        if @a[[x,y]][0].length == 4
          ret[4]+=",(#{x},#{y})"
        end
        y+=1
      end
      y = 0
      x+=1
    end
return ret
  end

  def paths()
    p = Hash.new
    ret = Array.new
    @path.each do |key, array|
      i = 0
      sum = 0
      x = array[0].to_i
      y = array[1].to_i
      if @a[[x,y]][1]!=nil
      nope = false
      while i < array[2].length && !nope
        if array[2][i] == "r"
          ws = @a[[x,y]][1].split(/\s/)
          n = @a[[x,y]][0].index("r")
          if !n
            nope = true
          else
            sum+=ws[n].to_f
          end
          x+=1
        elsif array[2][i] == "l"
          ws = @a[[x,y]][1].split(/\s/)
          n = @a[[x,y]][0].index("l")
          if !n
            nope = true
          else
            sum+=ws[n].to_f
          end
          x-=1
        elsif  array[2][i] == "d"
          ws = @a[[x,y]][1].split(/\s/)
          n = @a[[x,y]][0].index("d")
          if !n
            nope = true
          else
            sum+=ws[n].to_f
          end
          y+=1
        elsif array[2][i] == "u"
          ws = @a[[x,y]][1].split(/\s/)
          n = @a[[x,y]][0].index("u")
          if !n
            nope = true
          else
            sum+=ws[n].to_f
          end
          y-=1
        end
        i+=1
      end
      if !nope
        p[key] = sum
      end
    end
end
    pa = Array.new
    p.each do |key, value|
      pa.push([value.to_f,key])
    end

    pa.sort_by!{|x,y| x}

    pa.each do |index|
      if pa[pa.length] == index
         o = sprintf("%10.4f",index[0])
        ret.push("#{o} #{index[1]}")
      else
        o = sprintf("%10.4f",index[0])
       ret.push("#{o} #{index[1]}")
      end
    end
    if pa.length == 0
      return "none"
    else
      return ret
     end
    end

    def pathNoPrint()
      p = Hash.new
      shortest = Array.new
      @path.each do |key, array|
        i = 0
        sum = 0
        x = array[0].to_i
        y = array[1].to_i
        nope = false
        if @a[[x,y]][1]!=nil
        while i < array[2].length && !nope
          if array[2][i] == "r"
            ws = @a[[x,y]][1].split(/\s/)
            n = @a[[x,y]][0].index("r")
            if !n
              nope = true
            else
              sum+=ws[n].to_f
            end
            x+=1
          elsif array[2][i] == "l"
            ws = @a[[x,y]][1].split(/\s/)
            n = @a[[x,y]][0].index("l")
            if !n
              nope = true
            else
              sum+=ws[n].to_f
            end
            x-=1
          elsif  array[2][i] == "d"
            ws = @a[[x,y]][1].split(/\s/)
            n = @a[[x,y]][0].index("d")
            if !n
              nope = true
            else
              sum+=ws[n].to_f
            end
            y+=1
          elsif array[2][i] == "u"
            ws = @a[[x,y]][1].split(/\s/)
            n = @a[[x,y]][0].index("u")
            if !n
              nope = true
            else
              sum+=ws[n].to_f
            end
            y-=1
          end
          i+=1
        end
        if !nope
          p[key] = sum
        end
      end
    end

      pa = Array.new

      p.each do |key, value|
        value.round(4)
        pa.push(value)
      end
      pa.sort!

      pa.each do |index|
        if index == pa[0]
          shortest.push(@path[p.key(index)][0])
          shortest.push(@path[p.key(index)][1])
          shortest.push(@path[p.key(index)][2])
        end
      end
      i = 0
      x = shortest[0].to_i
      y = shortest[1].to_i
      while shortest[2] != nil && i <= shortest[2].length
        @shortpath.push("#{x} #{y}")
        if shortest[2][i] == "r"
          x+=1
        elsif shortest[2][i] == "l"
          x-=1
        elsif shortest[2][i] == "u"
          y-=1
        elsif shortest[2][i] == "d"
          y+=1
        end
        i+=1
      end
    end

    def printit(file)
      ret = Array.new
      x = 0
      y = 0
      ret.push("")
      if !@path.empty?
        pathNoPrint()
      end
      while y<=@sz do
        ret[0]+=("+")
        if y != @sz
          ret[0]+= "-"
        end
        y+=1
      end
      y=0
      ret[0]+="\n"
      while y<@sz do
        while x<@sz do
          if !@a[[x,y]][0].include?("l")
            ret[0]+= "|"
          else
            ret[0]+= "\s"
          end
        if x == @sx.to_i && y == @sy.to_i && @shortpath.include?("#{x} #{y}")
          ret[0]+= "S"
        elsif x == @ex.to_i && y == @ey.to_i && @shortpath.include?("#{x} #{y}")
          ret[0]+= "E"
        elsif x == @sx.to_i && y == @sy.to_i
          ret[0]+= "s"
          elsif x == @ex.to_i && y == @ey.to_i
            ret[0]+= "e"
          elsif @shortpath.include?("#{x} #{y}")
            ret[0]+=  "*"
          else ret[0]+= "\s"
          end
          x+=1
        end
        ret[0]+= "|\n"
        x=0
        while x<@sz do
        ret[0]+= "+"
          if !@a[[x,y]][0].include?("d")
            ret[0]+= "-"
          else
        ret[0]+= "\s"
          end
          x+=1
        end
        if y != @sz-1
          ret[0]+= "+\n"
        else
         ret[0]+= "+"
        end
        x = 0
        y+=1
      end
      return ret.join("\n")
    end

    def distance
      x = 0
      y = 0
      ret = Array.new
      ret.push("")
      bool = Hash.new()
      queue = Queue.new
      countHash = Hash.new
      queue.push("#{@sx} #{@sy}")
      while y<@sz do
        while x<@sz do
          bool[[x,y]] = false
          countHash[[x,y]] = 0
          x+=1
        end
        y+=1
        x=0
      end
      x = @sx.to_i
      y = @sy.to_i
      bool[[x,y]] = true
      count = -1
      while !queue.empty?
        ex = queue.pop
        x,y = ex.split(/ /)
        x = x.to_i
        y = y.to_i
        if count != countHash[[x,y]]
          if countHash[[x,y]] == 0
            ret[0]+=""
          else
          ret[0]+="\n"
        end
          count+=1
        end
        ret[0]+=",(#{x},#{y}) "

        if @a[[x,y]][0].include?("u") && !bool[[x,y-1]]
          countHash[[x,y-1]]=countHash[[x,y]]+1
          queue.push("#{x} #{y-1}")
          bool[[x,y-1]] = true
        end
        if @a[[x,y]][0].include?("l") && !bool[[x-1,y]]
          countHash[[x-1,y]]=countHash[[x,y]]+1
          queue.push("#{x-1} #{y}")
          bool[[x-1,y]] = true
        end
        if @a[[x,y]][0].include?("r") && !bool[[x+1,y]]
          countHash[[x+1,y]]=countHash[[x,y]]+1
          queue.push("#{x+1} #{y}")
          bool[[x+1,y]] = true
        end
        if @a[[x,y]][0].include?("d") && !bool[[x,y+1]]
          countHash[[x,y+1]]=countHash[[x,y]]+1
          queue.push("#{x} #{y+1}")
          bool[[x,y+1]] = true
        end
      end
      q = Array.new
      h = Array.new
      q = ret[0].split("\n")
      t = 0
      while t < q.length
        h = q[t].split("\s")
        h.sort!
        q[t] = h.join("")
        t+=1
      end
      t = 0
      while t < q.length
        q[t].prepend("#{t}")
        t+=1
      end
      return q.join("\n")
    end

    def solve(x,y)
      bool = Hash.new()
      stack = Array.new
      stack.push("#{@sx} #{@sy}")
      x = 0
      y = 0
      while y<@sz do
        while x<@sz do
          bool[[x,y]] = false
          x+=1
        end
        y+=1
        x=0
      end
      while stack.any?
        ex = stack.pop
        x,y = ex.split(/ /)
        x = x.to_i
        y = y.to_i
        if x == @ex.to_i && y == @ey.to_i
          return true
        end
        if @a[[x,y]][0].include?("u") && !bool[[x,y-1]]
          stack.push("#{x} #{y-1}")
          bool[[x,y-1]] = true
        end
        if @a[[x,y]][0].include?("l") && !bool[[x-1,y]]
          stack.push("#{x-1} #{y}")
          bool[[x-1,y]] = true
        end
        if @a[[x,y]][0].include?("r") && !bool[[x+1,y]]
          stack.push("#{x+1} #{y}")
          bool[[x+1,y]] = true
        end
        if @a[[x,y]][0].include?("d") && !bool[[x,y+1]]
          stack.push("#{x} #{y+1}")
          bool[[x,y+1]] = true
        end
      end
      return false
    end
    #----------------------------------
    def main(command_name, file_name)
      @a = Hash.new()
      @path = Hash.new()
      @sz, @sx, @sy, @ex, @ey = 0
      @shortpath = Array.new()

      maze_file = open(file_name)



      read_and_print_simple_file(maze_file)

      # perform command
      case command_name
      when "parse"
        parse(maze_file)
      when "open"
        opens(maze_file)
      when "bridge"
        bridge(maze_file)
      when "sortcells"
        sortcells(maze_file)
      when "paths"
        paths()
      when "print"
        printit(maze_file)
      when "distance"
        distance()
      when "solve"
        solve(@sx.to_i,@sy.to_i)
      else
        fail "Invalid command"
      end
    end
