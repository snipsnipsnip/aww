require 'infer'
require 'minitest/autorun'

describe Infer do
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
    
    it "foo" do
      assert_equal [:a, [:b, [:c, :b]]], infer([:^, :a, [[:^, :b, [:^, :c, [:^, :d, :c]]], :a]])
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
      
      assert_raises(Infer::LoopError) do
        infer [:^, :a, [[[:^, :b, [:^, :c, :a]], :a], [[:a, [[[:^, :d, :d], [:a, :a]], [:^, :e, [:^, :f, :f]]]], [:^, :g, :g]]]]
      end
    end
  end
end
