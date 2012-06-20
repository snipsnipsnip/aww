
class Infer
  attr_reader :cxt
  attr_writer :verbose

  def initialize(verbose=false)
    @n = 0
    @cxt = {}
    @verbose = verbose
  end
  
  def infer(expr, env)
    rewrite resolve(check(expr, env))
  end
  
  private
  
  def rewrite(type, dict={})
    if type.is_a?(Array)
      type.map {|t| rewrite(t, dict) }
    elsif type.is_a?(Symbol)
      type
    elsif type.is_a?(Fixnum)
      dict[type] ||= (dict.size + 10).to_s(36).to_sym
    else
      raise "unexpected type: #{type.inspect}"
    end
  end
  
  def resolve(n, ignore_loop=false, prev=nil)
    warn "resolve: #{n.inspect} => ?" if @verbose
    
    if prev && prev.include?(n)
      if ignore_loop && prev[-1] != n
        warn "ignoring loop (#{prev.join ' -> '} -> #{n}), tying knots" if @verbose
        return @cxt[n] = n
      else 
        raise LoopError, "loop detected! (#{prev.join ' -> '} -> #{n})"
      end
    end
    
    r = case n
    when Fixnum
      if (d = @cxt[n]) && n != d
        resolve(d, ignore_loop, (prev ||= []).push(n))
      else
        n
      end
    when Array
      n.map {|x| resolve(x, ignore_loop, prev) }
    when Symbol
      n
    when nil
      raise "definition not found (#{prev && prev.join(' -> ')})"
    else
      raise "unexpected query: #{n.inspect} (#{prev && prev.join(' -> ')})"
    end
    warn "resolve: #{n.inspect} => #{r.inspect}" if @verbose
    r
  end
  
  def newtype
    @n += 1
  end
  
  def unify(a, b)
    warn "unify: #{a.inspect} = #{b.inspect}" if @verbose
    
    a = resolve(a, true)
    b = resolve(b, true)
    
    if a.is_a?(Array) && b.is_a?(Array) && a.size == b.size
      a.zip(b) {|x, y| unify(x, y) }
    elsif a.is_a?(Fixnum) || b.is_a?(Fixnum)
      @cxt[a] ||= b if a.is_a?(Fixnum)
      @cxt[b] ||= a if b.is_a?(Fixnum)
    elsif a != b
      raise TypeMismatchError, "type mismatch: #{a.inspect} != #{b.inspect}"
    end
  end
  
  def check(expr, env)
    warn "check: #{expr.inspect} => ?" if @verbose
    r = case expr
    when Symbol
      env[expr] or raise "type not found for #{expr}"
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
    warn "check: #{expr.inspect} => #{r.inspect}" if @verbose
    r
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
