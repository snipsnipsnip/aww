
class Infer
  attr_reader :cxt
  attr_writer :verbose

  def initialize(verbose=false)
    @n = 0
    @cxt = {}
    @verbose = verbose
  end
  
  def infer(expr, env)
    make_typevar_readable resolve(check(expr, env))
  end
  
  private
  
  def make_typevar_readable(type)
    dict = {}
    rewrite(type) {|type| dict[type] ||= (dict.size + 10).to_s(36).to_sym }
  end
  
  def rewrite(type, &blk)
    if type.is_a?(Array)
      type.map {|t| rewrite(t, &blk) }
    elsif type.is_a?(Symbol)
      type
    elsif type.is_a?(Fixnum)
      yield(type)
    else
      raise Bug, "unexpected type: #{type.inspect}"
    end
  end
  
  def resolve(n, *prev)
    log "#{n.inspect} => ?" if @verbose
    
    if prev.include?(n)
      raise LoopError, "loop detected! (#{prev.join ' -> '} -> #{n})"
    end
    
    r = case n
    when Fixnum
      if d = @cxt[n]
        resolve(d, n, *prev)
      else
        n
      end
    when Array
      n.map {|x| resolve(x, *prev) }
    when Symbol
      n
    when nil
      raise Bug, "definition not found (#{prev.join(' -> ')})"
    else
      raise Bug, "unexpected query: #{n.inspect} (#{prev.join(' -> ')})"
    end
    log "#{n.inspect} => #{r.inspect}" if @verbose
    r
  end
  
  def newtype
    @n += 1
  end
  
  def unify(a, b)
    log "#{a.inspect} = #{b.inspect}" if @verbose
    
    a = resolve(a)
    b = resolve(b)
    
    if a.is_a?(Array) && b.is_a?(Array) && a.size == b.size
      a.zip(b) {|x, y| unify(x, y) }
    elsif a.is_a?(Fixnum) && b.is_a?(Fixnum)
      raise Bug, "unexpected entry" if @cxt.key?(a) || @cxt.key?(b)
      # as both are tip (local minimum), ensured by resolve, we can just join the set
      @cxt[a] = @cxt[b] = newtype
    elsif a.is_a?(Fixnum) || b.is_a?(Fixnum)
      a, b = b, a if b.is_a?(Fixnum)
      raise Bug, "unexpected entry" if @cxt.key?(a)
      @cxt[a] = b
    elsif a != b
      raise TypeMismatchError, "type mismatch: #{a.inspect} != #{b.inspect}"
    end
  end
  
  def refresh_typevars(t)
    dict = []
    rewrite(t) {|type| type >= 0 ? type : (dict[-type] ||= newtype) }
  end
  
  def check(expr, env)
    log "#{expr.inspect} => ?" if @verbose
    r = case expr
    when Symbol
      t = env[expr] or raise RefError, "type not found for #{expr}"
      refresh_typevars(t)
    when Array
      case expr[0]
      when :^
        args, body = expr[1..-1]
        args = Array(args)
        
        args.empty? and raise SyntaxError, "malformed lambda: #{expr.inspect}"
        
        local_env = env.dup
        types = args.map {|argname| local_env[argname] = newtype }
        type = check(body, local_env)
        types.reverse_each do |t|
          type = [t, type]
        end
        type
      else
        if expr.size > 2
          check([expr[0..-2], expr[-1]], env)
        elsif expr.size < 2
          raise SyntaxError, "unexpected syntax: #{expr.inspect}"
        else
          op, arg = expr
          to = check(op, env)
          ta = check(arg, env)
          r = newtype
          unify [ta, r], to
          r
        end
      end
    when Numeric
      :num
    end
    log "#{expr.inspect} => #{r.inspect}" if @verbose
    r
  end
  
  def log(msg)
    c = caller
    func = c[0][c[0].index('`').succ..-2]
    indent = c.size - (@initial ||= c.size)
    warn "#{' ' * indent}#{func}: #{msg}"
  end
  
  class Error < RuntimeError
  end
  
  class Bug < Error
  end
  
  class TypeMismatchError < Error
  end
  
  class LoopError < Error
  end
  
  class RefError < Error
  end
  
  class SyntaxError < Error
  end
end

module ExprUtil
  module_function
  def check(expr)
    stack = [expr]
    
    while e = stack.pop
      next unless e.is_a?(Array)
      
      if e.size < 2
        raise Infer::SyntaxError, "unexpected array size: #{e.inspect}"
      elsif :^ == e[0]
        e.size == 3 and
        (e[1].is_a?(Symbol) or
          (e[1].is_a?(Array) and
          e[1].size > 1 and
          e[1].all? {|x| x.is_a? Symbol })) or
          raise Infer::SyntaxError, "malformed lambda: #{e.inspect}"
        stack.push e[2]
      else
        stack.concat e
      end
    end
  end

  def pretty(e)
    case e
    when Symbol
      e
    else
      case e[0]
      when :^
        "(\\#{e[1]} -> #{prettye e[2]})"
      else
        "(#{prettye e[0]} #{prettye e[1]})"
      end
    end
  end
  
  def generate(depth=10)
    ExpGen.new.gen(depth)
  end
  
  class ExpGen
    def initialize
      @n = 0
    end
    
    def var(n=(@n+=1))
      (n + 9).to_s(36).to_sym
    end
    
    def gen(depth=rand(10), *vars)
      case depth < 0 ? 2 : depth == 0 && vars.empty? ? 1 : rand(vars.empty? ? 2 : 3)
      when 0
        [gen(depth - 1, *vars), gen(depth - 1, *vars)]
      when 1
        v = var
        [:^, v, gen(depth - 1, v, *vars)]
      else
        vars[rand(vars.size)]
      end
    end
  end
end

if $0 == __FILE__
  ARGV.each do |a|
    p Infer.new(true).infer(eval(a), {})
  end
end
