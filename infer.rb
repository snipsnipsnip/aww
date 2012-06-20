
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
      dict[type] ||= (dict.size + ?a).chr.to_sym
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

$:.concat %w[
  c:/local/ruby-1.8/lib/ruby/gems/1.8/gems/minitest-3.1.0/lib
]
require 'minitest/autorun'

describe "infer" do
  let :default_env do
    {
      :cons => [:num, [:list, :list]],
      :car => [:list, :list],
      :cdr => [:list, :list],
      :null => [:list, :bool],
      :nil => :list
    }
  end
  
  def infer(expr, env=default_env)
    Infer.new(@verbose).infer(expr, env)
  end
  
  def report
    @verbose = true
    yield
    @verbose = false    
  end
  
  describe "vars" do
    it "should be typed as in context" do
      default_env.each do |k,v|
        assert_equal v, infer(k)
      end
    end
  end

  describe "numbers" do
    it "should be typed as num" do
      assert_equal :num, infer(3)
    end
  end
  
  describe "app" do
    it "should be typed from result" do
      assert_equal :list, infer([:car, :nil])
      assert_equal :list, infer([:cdr, :nil])
      assert_equal :bool, infer([:null, :nil])
      assert_equal [:list, :list], infer([:cons, 1])
      assert_equal :list, infer([:cons, 1, :nil])
    end
    
    it "should raise error on arg type mismatch" do
      assert_raises(Infer::TypeMismatchError) { infer([:car, 1]) }
      assert_raises(Infer::TypeMismatchError) { infer([:cdr, 1]) }
      assert_raises(Infer::TypeMismatchError) { infer([:null, 1]) }
    end
    
    it "should detect non-function app" do
      assert_raises(Infer::TypeMismatchError) { infer([:nil, 1]) }
      assert_raises(Infer::TypeMismatchError) { infer([:nil, :car]) }
      assert_raises(Infer::TypeMismatchError) { infer([4, :car]) }
      assert_raises(Infer::TypeMismatchError) { infer([:null, :nil, :nil]) }
      assert_raises(Infer::TypeMismatchError) { infer([:cons, :nil, :nil, :nil]) }
    end
    
    it "shouldn't be able to type higher-rank stuff" do
      assert_raises(Infer::TypeMismatchError) { infer([:cons, :nil]) }
      assert_raises(Infer::TypeMismatchError) { infer([:cons, :cons]) }
    end
  end
  
  describe "abs" do
    it "should type constant function" do
      assert_equal [:a, :num], infer([:^, :x, 1])
      default_env.each do |k,v|
        assert_equal [:a, v], infer([:^, :x, k])
      end
    end
    
    it "should type some combinators" do
      assert_equal [:a, :a], infer([:^, :x, :x])
      assert_equal [:a, [:b, :a]], infer([:^, :x, [:^, :y, :x]])
      assert_equal [:a, [:b, :b]], infer([:^, :x, [:^, :y, :y]])
      assert_equal [:a, [:b, :num]], infer([:^, :x, [:^, :y, 3]])
      assert_equal [[:a, [:b, :c]], [[:a, :b], [:a, :c]]], infer([:^, :x, [:^, :y, [:^, :z, [[:x, :z], [:y, :z]]]]])
      
      assert_equal [:a, :a], infer([
        [:^, :x, [:^, :y, [:^, :z, [[:x, :z], [:y, :z]]]]],
        [:^, :x, [:^, :y, :x]],
        [:^, :x, [:^, :y, :x]]
      ])
    end
    
    it "hmm" do
      assert_equal :num, infer([[:^, :x, :x], 3])
      assert_equal [:a, :a], infer([[:^, :x, :x], [:^, :x, :x]])
      assert_equal [:num, [:list, :list]], infer([[:^, :x, :x], :cons])
      assert_equal :list, infer([[:^, :x, :x], :cons, 10, :nil])
      assert_equal [:a, :a], infer([[:^, :x, :x], [:^, :x, :x], [:^, :x, :x]])
    end
    
    it "should induct" do
      assert_equal [:list, :bool], infer([:^, :x, [:null, :x]])
      assert_equal :bool, infer([[:^, :x, [:null, :x]], :nil])
      assert_equal [:list, :list], infer([:^, :x, [:car, :x]])
      assert_equal [:list, :list], infer([:^, :x, [:cdr, :x]])
      assert_equal [:num, [:list, :list]], infer([:^, :x, [:cons, :x]])
      assert_equal [:num, [:list, :list]], infer([:^, :x, [:^, :y, [:cons, :x, :y]]])
      assert_equal [:list, [:num, :list]], infer([:^, :x, [:^, :y, [:cons, :y, :x]]])
      assert_equal [[:a, :b], [:a, :b]], infer([:^, :x, [:^, :y, [:x, :y]]])
      assert_equal [:a, [[:a, :b], :b]], infer([:^, :x, [:^, :y, [:y, :x]]])
    end
    
    it "should deduct" do
      assert_equal [[:num, :a], :a], infer([:^, :x, [:x, 3]])
      assert_equal [[[:list, :list], :a], :a], infer([:^, :x, [:x, :car]])
      assert_equal [[:num, [:num, :a]], :a], infer([:^, :x, [:x, 3, 4]])
      assert_equal [[:num, [:list, :a]], :a], infer([:^, :x, [:x, 3, :nil]])
      assert_equal [[:list, [:num, :a]], :a], infer([:^, :x, [:x, :nil, 3]])
    end
    
    it "should detect mismatch" do
      assert_raises(Infer::TypeMismatchError) { infer([:^, :x, [:nil, :x]]) }
      assert_raises(Infer::TypeMismatchError) do
        infer([:s, :i, :i], {
          :i => [:num, :num],
          :s => [[:num, :num, :num], [[:num, :num], [:num, :num]]]
        })
      end
    end
    
    it "should detect loop" do
      assert_raises(Infer::LoopError) { infer([:^, :x, [:x, :x]]) }
      assert_raises(Infer::LoopError) { infer([:^, :x, [:x, :x, :x]]) }
      assert_raises(Infer::LoopError) { infer([:^, :x, [:x, :x, :x, :x]]) }
      assert_raises(Infer::LoopError) { infer([:^, :x, [:^, :y, [:x, :x]]]) }
      assert_raises(Infer::LoopError) { infer([:^, :x, [:^, :y, [:y, :x, :y]]]) }
    end
  end
end
