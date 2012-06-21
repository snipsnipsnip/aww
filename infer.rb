
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
      raise "unexpected type: #{type.inspect}"
    end
  end
  
  def resolve(n, *prev)
    log "#{n.inspect} => ?" if @verbose
    
    if prev.include?(n)
      raise LoopError, "loop detected! (#{prev.join ' -> '} -> #{n})"
    end
    
    r = case n
    when Fixnum
      if (d = @cxt[n]) && n != d
        resolve(d, n, *prev)
      else
        n
      end
    when Array
      n.map {|x| resolve(x, *prev) }
    when Symbol
      n
    when nil
      raise "definition not found (#{prev.join(' -> ')})"
    else
      raise "unexpected query: #{n.inspect} (#{prev.join(' -> ')})"
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
      raise "unexpected entry" if @cxt.key?(a) || @cxt.key?(b)
      # as both are tip (local minimum), ensured by resolve, we can just join the set
      @cxt[a] = @cxt[b] = newtype
    elsif a.is_a?(Fixnum) || b.is_a?(Fixnum)
      a, b = b, a if b.is_a?(Fixnum)
      raise "unexpected entry" if @cxt.key?(a)
      @cxt[a] = b if a.is_a?(Fixnum)
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
      t = env[expr] or raise "type not found for #{expr}"
      refresh_typevars(t)
    when Array
      case expr[0]
      when :^
        var, body = expr[1..-1]
        tv = newtype
        env2 = env.dup
        env2[var] = tv
        [tv, check(body, env2)]
      else
        if expr.size > 2
          check([expr[0..-2], expr[-1]], env)
        elsif expr.size < 2
          raise "unexpected syntax: #{expr.inspect}"
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
  
  class TypeMismatchError < RuntimeError
  end
  
  class LoopError < RuntimeError
  end
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
      [:^, v=var, gen(depth - 1, v, *vars)]
    else
      vars[rand(vars.size)]
    end
  end
end


def prettye(e)
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

if $0 == __FILE__
  ARGV.each do |a|
    p Infer.new(true).infer(eval(a), {})
  end
end
