require 'infer'
require 'rubygems'
require 'shoulda'

class InferTest < Test::Unit::TestCase
  def default_env
    @default_env ||= {
      :cons => [:num, [:list, :list]],
      :car => [:list, :num],
      :cdr => [:list, :list],
      :null => [:list, :bool],
      :nil => :list
    }
  end
  
  def infer(expr, env=default_env)
    ExprUtil.check expr
    Infer.new(@verbose ||= false).infer(expr, env)
  end
  
  def report
    @verbose = true
    yield
    @verbose = false    
  end

  context "concrete" do
    context "vars" do
      should "be typed as in context" do
        default_env.each do |k,v|
          assert_equal v, infer(k)
        end
      end
    end

    context "numbers" do
      should "be typed as num" do
        assert_equal :num, infer(3)
      end
    end
    
    context "app" do
      should "be typed from result" do
        assert_equal :num, infer([:car, :nil])
        assert_equal :list, infer([:cdr, :nil])
        assert_equal :bool, infer(true)
        assert_equal [:list, :list], infer([:cons, 1])
        assert_equal :list, infer([:cons, 1, :nil])
      end
      
      should "raise error on arg type mismatch" do
        assert_raises(Infer::TypeMismatchError) { infer([:car, 1]) }
        assert_raises(Infer::TypeMismatchError) { infer([:cdr, 1]) }
        assert_raises(Infer::TypeMismatchError) { infer([:null, 1]) }
      end
      
      should "detect non-function app" do
        assert_raises(Infer::TypeMismatchError) { infer([:nil, 1]) }
        assert_raises(Infer::TypeMismatchError) { infer([:nil, :car]) }
        assert_raises(Infer::TypeMismatchError) { infer([4, :car]) }
        assert_raises(Infer::TypeMismatchError) { infer([:null, :nil, :nil]) }
        assert_raises(Infer::TypeMismatchError) { infer([:cons, :nil, :nil, :nil]) }
      end
      
      should "shouldn't be able to type polymorphic stuff" do
        assert_raises(Infer::TypeMismatchError) { infer([:cons, :nil]) }
        assert_raises(Infer::TypeMismatchError) { infer([:cons, :cons]) }
      end
    end
    
    context "abs" do
      should "type constant function" do
        assert_equal [:a, :num], infer([:^, :x, 1])
        default_env.each do |k,v|
          assert_equal [:a, v], infer([:^, :x, k])
          assert_equal [:a, [:b, v]], infer([:^, :x, [:^, :y, k]])
        end
      end
      
      should "type some combinators" do
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
      
      should "handle some complex expressions" do
        assert_equal :num, infer([[:^, :x, :x], 3])
        assert_equal [:a, :a], infer([[:^, :x, :x], [:^, :x, :x]])
        assert_equal [:num, [:list, :list]], infer([[:^, :x, :x], :cons])
        assert_equal :list, infer([[:^, :x, :x], :cons, 10, :nil])
        assert_equal [:a, :a], infer([[:^, :x, :x], [:^, :x, :x], [:^, :x, :x]])
        assert_equal [:a, [:b, [[:b, :c], [:d, :c]]]], infer([:^, :a, [:^, :b, [[:^, :c, [:^, :d, [:^, :e, [:d, :b]]]], [:^, :f, [:^, :g, :f]]]]])
        assert_equal [:a, [:b, [:c, :b]]], infer([:^, :a, [[:^, :b, [:^, :c, [:^, :d, :c]]], :a]])
        assert_equal [:a, [[:b, :c], [[[:d, [:e, [[[:b, :c], [:c, :f]], :f]]], :g], [:b, :g]]]], infer([:^, :a, [:^, :b, [:^, :c, [:^, :d, [:c, [:^, :e, [:^, :f, [:^, :g, [[:g, :b], [:b, :d]]]]]]]]]])
      end
      
      should "induct" do
        assert_equal [:list, :bool], infer([:^, :x, [:null, :x]])
        assert_equal :bool, infer([[:^, :x, [:null, :x]], :nil])
        assert_equal [:list, :num], infer([:^, :x, [:car, :x]])
        assert_equal [:list, :list], infer([:^, :x, [:cdr, :x]])
        assert_equal [:num, [:list, :list]], infer([:^, :x, [:cons, :x]])
        assert_equal [:num, [:list, :list]], infer([:^, :x, [:^, :y, [:cons, :x, :y]]])
        assert_equal [:list, [:num, :list]], infer([:^, :x, [:^, :y, [:cons, :y, :x]]])
        assert_equal [[:a, :b], [:a, :b]], infer([:^, :x, [:^, :y, [:x, :y]]])
        assert_equal [:a, [[:a, :b], :b]], infer([:^, :x, [:^, :y, [:y, :x]]])
      end
      
      should "deduct" do
        assert_equal [[:num, :a], :a], infer([:^, :x, [:x, 3]])
        assert_equal [[[:list, :num], :a], :a], infer([:^, :x, [:x, :car]])
        assert_equal [[:num, [:num, :a]], :a], infer([:^, :x, [:x, 3, 4]])
        assert_equal [[:num, [:list, :a]], :a], infer([:^, :x, [:x, 3, :nil]])
        assert_equal [[:list, [:num, :a]], :a], infer([:^, :x, [:x, :nil, 3]])
      end
      
      should "detect mismatch" do
        assert_raises(Infer::TypeMismatchError) { infer([:^, :x, [:nil, :x]]) }
        assert_raises(Infer::TypeMismatchError) do
          infer([:s, :i, :i], {
            :i => [:num, :num],
            :s => [[:num, :num, :num], [[:num, :num], [:num, :num]]]
          })
        end
      end
      
      should "detect loop" do
        assert_raises(Infer::LoopError) { infer([:^, :x, [:x, :x]]) }
        assert_raises(Infer::LoopError) { infer([:^, :x, [:x, :x, :x]]) }
        assert_raises(Infer::LoopError) { infer([:^, :x, [:x, :x, :x, :x]]) }
        assert_raises(Infer::LoopError) { infer([:^, :x, [:^, :y, [:x, :x]]]) }
        assert_raises(Infer::LoopError) { infer([:^, :x, [:^, :y, [:y, :x, :y]]]) }
        assert_raises(Infer::LoopError) { infer([:^, :f, [[:^, :x, [:f, [:x, :x]]], [:^, :x, [:f, [:x, :x]]]]]) }
        
        assert_raises(Infer::LoopError) do
          infer [:^, :a, [[[:^, :b, [:^, :c, :a]], :a], [[:a, [[[:^, :d, :d], [:a, :a]], [:^, :e, [:^, :f, :f]]]], [:^, :g, :g]]]]
        end
      end
    end
  end
  
  context "mono" do
    context "combinators" do
      setup do
        default_env[:i] = [-1, -1]
        default_env[:s] = [[-1, [-2, -3]], [[-1, -2], [-1, -3]]]
        default_env[:k] = [-1, [-2, -1]]
        default_env[:c] = [[-1, [-2, -3]], [-2, [-1, -3]]]
        default_env[:kons] = [-1, [-2, [[-1, [-2, -3]], -3]]]
        default_env[:fix] = [[-1, -1], -1]
      end
      
      should "fix" do
        assert_equal [:a, :a], infer(:i)
        assert_equal [[:a, [:b, :c]], [[:a, :b], [:a, :c]]], infer(:s)
        assert_equal [:a, [:b, :a]], infer(:k)
        assert_equal [[:a, [:b, :c]], [:b, [:a, :c]]], infer(:c)
      end
      
      should "combinations" do
        assert_equal [:a, [:b, :b]], infer([:k, :i])
        assert_equal [:a, [:b, :a]], infer([:i, :k])
        assert_equal [:a, [:b, :b]], infer([:k, :i])
        assert_equal [:a, :a], infer([:i, :i])
        assert_equal [:a, :a], infer([:i, :i, :i])
        assert_equal [:a, :a], infer([:s, :k, :k])
        assert_equal [[[:a, :b], :a], [[:a, :b], :b]], infer([:s, :i])
        assert_equal [[[:a, [:b, :c]], [:a, :b]], [[:a, [:b, :c]], [:a, :c]]], infer([:s, :s])
      end
      
      should "mono" do
        assert_raises(Infer::TypeMismatchError) do
          infer [[:^, :f, [:kons, [:f, 3], [:f, true]]], :id]
        end
      end
    end
  end
  
  context "poly" do
    setup do
      @default_env = {
        :cons => [-1, [[:list, :of, -1], [:list, :of, -1]]],
        :car => [[:list, :of, -1], -1],
        :cdr => [[:list, :of, -1], [:list, :of, -1]],
        :null => [[:list, :of, -1], :bool],
        :nil => [:list, :of, -1],
        :fix => [[-1, -1], -1],
        :ifelse => [:bool, [-1, [-1, -1]]],
      }
    end
    
    should "adapt" do
      assert_equal [[:list, :of, :num], [:list, :of, :num]], infer([:cons, 1])
      assert_equal [[:list, :of, [:list, :of, :a]], [:list, :of, [:list, :of, :a]]], infer([:cons, :nil])
      assert_equal [[:list, :of, [:a, [[:list, :of, :a], [:list, :of, :a]]]], [:list, :of, [:a, [[:list, :of, :a], [:list, :of, :a]]]]], infer([:cons, :cons])
      assert_equal [:list, :of, :num], infer([:fix, [:cons, 1]])
      assert_equal [:list, :of, :bool], infer([:fix, [:cons, true]])
    end
    
    should "match" do
      assert_raises(Infer::TypeMismatchError) do
        infer [:cons, 1, [:cons, true, :nil]]
      end
      assert_raises(Infer::TypeMismatchError) { infer [:car, :ifelse] }
      assert_raises(Infer::TypeMismatchError) { infer [:ifelse, 1] }
      assert_raises(Infer::TypeMismatchError) { infer [:null, :fix] }
    end
    
    should "map" do
      assert_equal [[:a, :b], [[:list, :of, :a], [:list, :of, :b]]],
        infer([:fix,
          [:^, [:rec, :f, :xs],
            [:ifelse, [:null, :xs],
              :nil,
              [:cons, [:f, [:car, :xs]],
                      [:rec, :f, [:cdr, :xs]]]]]])

      assert_equal [[:a, :a], [[:list, :of, :a], [:list, :of, :a]]],
        infer([:fix,
          [:^, [:rec, :f, :xs],
            [:ifelse, [:null, :xs],
              :xs,
              [:cons, [:f, [:car, :xs]],
                      [:rec, :f, [:cdr, :xs]]]]]])
    end
    
    should "filter" do
      assert_equal [[:a, :bool], [[:list, :of, :a], [:list, :of, :a]]],
        infer([:fix,
          [:^, [:rec, :pred, :xs],
            [:ifelse, [:null, :xs],
              :nil,
              [:ifelse, [:pred, [:car, :xs]],
                [:cons, [:car, :xs],
                        [:rec, :pred, [:cdr, :xs]]],
                [:rec, :pred, [:cdr, :xs]]]]]])
    end
    
    should "foldl" do
      assert_equal [[:a, [:b, :a]], [:a, [[:list, :of, :b], :a]]],
        infer([:fix,
          [:^, :rec,
            [:^, [:f, :z, :xs],
              [:ifelse, [:null, :xs],
                :z,
                [:rec, :f, [:f, :z, [:car, :xs]],
                           [:cdr, :xs]]]]]])
    end
    
    should "foldr" do
      assert_equal [[:a, [:b, :b]], [:b, [[:list, :of, :a], :b]]],
        infer([:fix,
          [:^, [:rec, :f, :z, :xs],
            [:ifelse, [:null, :xs],
              :z,
              [:f, [:car, :xs],
                   [:rec, :f, :z, [:cdr, :xs]]]]]])
    end
    
    should "append" do
      assert_equal [[:list, :of, :a], [[:list, :of, :a], [:list, :of, :a]]],
        infer([:fix,
          [:^, :rec,
            [:^, :xs,
              [:^, :ys,
                [:ifelse, [:null, :xs],
                  :ys,
                  [:cons, [:car, :xs],
                          [:rec, [:cdr, :xs], :ys]]]]]]])
      
      e = default_env.merge({ :foldr => [[-1, [-2, -2]], [-2, [[:list, :of, -1], -2]]] })
      assert_equal [[:list, :of, :a], [[:list, :of, :a], [:list, :of, :a]]],
        infer([:foldr, :cons], e)
    end
  
    context "alternative" do
      setup do
        default_env.clear
        default_env[:append] = [[-1], [[-1], [-1]]]
        default_env[:wrap] = [-1, [-1]]
      end
      
      should "hoge" do
        assert_equal [[[:a], [[:a], [:a]]]], infer([:wrap, :append])
      end
    end
  
    context "let" do
      setup do
        default_env[:pair] = [-1, [-2, [:pair, -1, -2]]]
        default_env[:id] = [-1, -1]
      end
    
      should "keep poly" do
        assert_equal [:pair, :bool, :num], infer([:let, :f, :car,
          [:pair, [:f, [:cons, true, :nil]],
                  [:f, [:cons, 100, :nil]]]])
        
        assert_equal [:pair, :bool, :num], infer([:let, :f, [:id, :car],
          [:pair, [:f, [:cons, true, :nil]],
                  [:f, [:cons, 100, :nil]]]])

        assert_equal [:pair, :bool, :num], infer([:let, :f, [:^, :x, [:car, :x]],
          [:pair, [:f, [:cons, true, :nil]],
                  [:f, [:cons, 100, :nil]]]])
        
        assert_equal [:pair, [:pair, :bool, :bool], [:pair, :num, :num]], infer([:let, :f, [:^, :x, [:pair, :x, :x]],
          [:pair, [:f, true],
                  [:f, 100]]])
      end
      
      should "partially mono" do
        assert_equal [:bool, [:pair, [:a, [:pair, [:list, :of, :bool], :a]], [:b, [:pair, [:list, :of, :bool], :b]]]], infer(
          [:^, :x, 
            [:let, :g, [:^, [:y, :z], [:pair, [:cons, :x, [:cons, :y, :nil]], :z]],
              [:pair, [:g, true], [:g, false]]]])
        
        assert_raises(Infer::TypeMismatchError) do
          infer([:^, :x, 
            [:let, :g, [:^, [:y, :z], [:pair, [:cons, :x, [:cons, :y, :nil]], :z]],
              [:pair, [:g, true], [:g, 3]]]])
        end
      end
      
      should "rec" do
        assert_equal :a, infer([:let, :x, :x, :x])
        assert_equal [:list, :of, :num], infer([:let, :x, [:cons, 1, :x], :x])
        assert_equal [:list, :of, [[:list, :of, :a], :a]], infer([:let, :x, [:cons, :car, [:cons, :car, :x]], :x])
      
        assert_equal [[:list, :of, :a], [[:list, :of, :a], [:list, :of, :a]]],
          infer([:let, :rec,
            [:^, [:xs, :ys],
              [:ifelse, [:null, :xs],
                :ys,
                [:cons, [:car, :xs],
                        [:rec, [:cdr, :xs], :ys]]]],
            :rec])
      end
    end
    
    context "annot" do
      should "simple" do
        assert_equal :bool, infer([:cast, :bool, true])
        assert_equal :num, infer([:cast, :num, 100])
        assert_raises(Infer::TypeMismatchError) { infer([:cast, :bool, 3]) }
        assert_raises(Infer::TypeMismatchError) { infer([:cast, [:num, :num], 100]) }
        assert_equal :bool, infer([:cast, :bool, [:let, :a, :a, :a]])
        assert_equal [:bool, :bool], infer([:cast, [:bool, :bool], [:let, :a, :a, :a]])
      end
      
      should "func" do
        assert_equal [:a, :a], infer([:cast, [1, 1], [:^, :x, :x]])
        assert_equal [:bool, :bool], infer([:cast, [:bool, :bool], [:^, :x, :x]])
        assert_equal [:num, :num], infer([:cast, [:num, :num], [:^, :x, :x]])
        assert_equal [:a, [:a, :a]], infer([:cast, [1, [1, 1]], [:^, [:x, :y], :x]])
        assert_raises(Infer::TypeMismatchError) { infer([:cast, [:num, :bool], [:^, :x, :x]]) }
        assert_raises(Infer::TypeMismatchError) { infer([[:cast, [:num, :num], [:^, :x, :x]], true]) }
      end
    end
  end
end
