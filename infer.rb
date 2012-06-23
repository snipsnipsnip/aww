
class Infer
  attr_accessor :verbose

  def initialize(verbose=false)
    @n = -1
    @cxt = []
    @verbose = verbose
  end
  
  def infer(expr, env)
    log "#{expr.inspect} in #{env.inspect}" if @verbose
    make_typevar_readable resolve(infer_raw(expr, env))
  end
  
  private
  
  def infer_raw(expr, env)
    texpr = newtype
    check(texpr, expr, env)
    texpr
  end
  
  def make_typevar_readable(type)
    dict = {}
    name = "`"
    rewrite(type) {|type| dict[type] ||= (name = name.succ).to_sym }
  end
  
  def instantiate_generic_vars(t)
    dict = []
    rewrite(t) {|type| type >= 0 ? type : (dict[- type - 1] ||= newtype) }
  end
  
  def refresh_typevars(t)
    dict = []
    rewrite(t) {|type| type < 0 ? type : (dict[type] ||= newtype) }
  end
  
  def rewrite(type, &blk)
    if type.is_a?(Array)
      type.map {|t| rewrite(t, &blk) }
    elsif type.is_a?(Symbol)
      type
    elsif type.is_a?(Fixnum)
      yield(type)
    elsif type.class == Object
      type
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
      unless n.class == Object
        raise Bug, "unexpected query: #{n.inspect} (#{prev.join(' -> ')})"
      end
      n
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
      raise Bug, "unexpected entry" if @cxt[a] || @cxt[b]
      # as both are tip (local minimum), ensured by resolve, we can just join the set
      @cxt[a] = @cxt[b] = newtype
    elsif a.is_a?(Fixnum) || b.is_a?(Fixnum)
      a, b = b, a if b.is_a?(Fixnum)
      raise Bug, "unexpected entry" if @cxt[a]
      @cxt[a] = b
    elsif a != b
      raise TypeMismatchError, "type mismatch: #{a.inspect} != #{b.inspect}"
    end
  end
  
  def unify_as_function(expected_type)
    expected_type = resolve(expected_type)
    if expected_type.is_a?(Array) && expected_type.size == 2
      expected_type
    else
      pair = [newtype, newtype]
      unify pair, expected_type
      pair
    end
  end
  
  def check(expected_type, expr, env)
    log "#{expr.inspect} v #{expected_type.inspect}" if @verbose
    case expr
    when Symbol
      t = env[expr] or raise RefError, "type not found for #{expr}"
      unify expected_type, instantiate_generic_vars(t)
    when Array
      if expr.size < 2
        raise SyntaxError, "unexpected syntax: #{expr.inspect}"
      end
      
      case expr[0]
      when :^
        check_abs(expected_type, expr, env)
      when :let
        check_let(expected_type, expr, env)
      when :cast
        check_annot(expected_type, expr, env)
      else
        check_app(expected_type, expr, env)
      end
    when Numeric
      unify expected_type, :num
    when TrueClass, FalseClass
      unify expected_type, :bool
    end
    log "#{expr.inspect} ^ #{resolve(expected_type).inspect}" if @verbose
  end
  
  def generalize(type, env)
    type = resolve(type)
    ftv = collect_free_type_variables(env)
    log "#{type.inspect} in #{ftv.inspect}" if @verbose
    dict = {}
    r = rewrite(type) do |t|
      next t if ftv.include?(t)
      dict[t] ||= -(dict.size + 1)
    end
    log "#{r.inspect}" if @verbose
    r
  end
  
  def collect_free_type_variables(xs)
    ftv = xs.is_a?(Hash) ? xs.values : [xs]
    ftv.flatten!
    ftv.map! {|x| resolve(x) if x.is_a?(Fixnum) && x >= 0 }
    ftv.compact!
    ftv
  end
  
  def check_let(expected_type, expr, env)
    var, expr, body = expr[1..-1]
    local_env = env.dup
    tvar = newtype
    local_env[var] = tvar
    check(tvar, expr, local_env)
    local_env[var] = generalize(tvar, local_env)
    check(expected_type, body, local_env)
  end
  
  def check_app(expected_type, expr, env)
    top = newtype
    if expr.size > 2
      check_app(top, expr[0..-2], env)
    else
      check(top, expr[0], env)
    end
    targ, tresult = unify_as_function(top)
    check(targ, expr[-1], env)
    unify(tresult, expected_type)
  end
  
  def check_abs(expected_type, expr, env)
    targ, tresult = unify_as_function(expected_type)
    local_env = env.dup
    
    args, body = expr[1..-1]
    single = args.is_a?(Symbol)
    varg = single ? args : args[0]
    local_env[varg] = targ
    
    if single || args.size == 1
      check(tresult, expr[2], local_env)
    else
      check_abs(tresult, [:^, args[1..-1], body], local_env)
    end
  end
  
  def check_annot(expected_type, expr, env)
    tannot, body = expr[1..-1]
    tannot = refresh_typevars(tannot)
    tbody = infer_raw(body, env)
    unify_generic(tbody, tannot)
    unify(expected_type, instantiate_generic_vars(tannot))
  end
  
  def unify_generic(a, b)
    if generic?(b)
      skolem_vars, b2 = skolemise(b)
      unify_generic(a, b2)
      if !skolem_vars.empty? && collect_free_type_variables(a).any? {|v| skolem_vars.include?(v) }
        raise TypeMismatchError, "#{a.inspect} is more polymorphic than #{b.inspect}"
      end
    elsif generic?(a)
      unify_generic instantiate_generic_vars(a), b
    else
      unify a, b
    end
  end
  
  def generic?(type)
    if type.is_a?(Array)
      type.any? {|x| generic? x }
    elsif type.is_a?(Fixnum) && type < 0
      true
    else
      false
    end
  end
  
  def skolemise(sigma)
    vars = []
    t = rewrite(sigma) do |type|
      next type if type >= 0
      skolem = Object.new
      vars << skolem
      skolem
    end
    [vars, t]
  end
  
  def log(msg)
    c = caller
    func = c[0][c[0].index('`').succ..-2]
    indent = c.size - (@initial ||= c.size)
    warn "#{' ' * [indent, 0].max}#{func}: #{msg}"
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
      elsif :let == e[0]
        e.size == 4 and
        e[1].is_a?(Symbol) or
          raise Infer::SyntaxError, "malformed let: #{e.inspect}"
        stack << e[2] << e[3]
      elsif :cast == e[0]
        e.size == 3 or
        stack << e[2]
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
        "(\\#{e[1]} -> #{pretty e[2]})"
      when :let
        "(let #{e[1]} = #{pretty e[2]} in #{e[3..-1].map {|x| pretty x }.join(' ')})"
      when :cast
        "(#{pretty e[2]} :: #{pretty e[1]})"
      else
        "(#{e.map {|x| pretty x }.join(' ')})"
      end
    end
  end
  
  def generate(depth=10)
    ExpGen.new.gen(depth)
  end
  
  class ExpGen
    def initialize
      @n = "`"
    end
    
    def var
      (@n = @n.succ).to_sym
    end
    
    def gen(depth=rand(10), *vars)
      case depth < 0 ? 2 : depth == 0 && vars.empty? ? 1 : rand(vars.empty? ? 2 : 3)
      when 0
        [gen(depth - 1, *vars), gen(depth - 1, *vars)]
      when 1
        v = var
        if rand(2) == 0
          [:^, v, gen(depth - 1, v, *vars)]
        else
          [:let, v, gen(depth - 1, v, *vars), gen(depth - 1, v, *vars)]
        end
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
