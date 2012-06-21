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
    Infer.new(@verbose ||= false).infer(expr, env)
  end
  
  def report
    @verbose = true
    yield
    @verbose = false    
  end

  context "mono" do
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
        assert_equal :bool, infer([:null, :nil])
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
      
      should "shouldn't be able to type higher-rank stuff" do
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
          infer [[:^, :f, [:kons, [:f, 3], [:f, [:null, :nil]]]], :kons]
        end
      end
    end
    
    context "poly" do
      setup do
        default_env.clear
        default_env[:cons] = [-1, [[:list, :of, -1], [:list, :of, -1]]]
        default_env[:car] = [[:list, :of, -1], -1]
        default_env[:cdr] = [[:list, :of, -1], [:list, :of, -1]]
        default_env[:null] = [[:list, :of, -1], :bool]
        default_env[:nil] = [:list, :of, -1]
        default_env[:fix] = [[-1, -1], -1]
        default_env[:ifelse] = [:bool, [-1, [-1, -1]]]
      end
      
      should "adapt" do
        assert_equal [[:list, :of, :num], [:list, :of, :num]], infer([:cons, 1])
        assert_equal [[:list, :of, [:list, :of, :a]], [:list, :of, [:list, :of, :a]]], infer([:cons, :nil])
        assert_equal [[:list, :of, [:a, [[:list, :of, :a], [:list, :of, :a]]]], [:list, :of, [:a, [[:list, :of, :a], [:list, :of, :a]]]]], infer([:cons, :cons])
        assert_equal [:list, :of, :num], infer([:fix, [:cons, 1]])
        assert_equal [:list, :of, :bool], infer([:fix, [:cons, [:null, :nil]]])
      end
      
      should "match" do
        assert_raises(Infer::TypeMismatchError) do
          infer [:cons, 1, [:cons, [:null, :nil], :nil]]
        end
        assert_raises(Infer::TypeMismatchError) { infer [:car, :ifelse] }
        assert_raises(Infer::TypeMismatchError) { infer [:ifelse, 1] }
        assert_raises(Infer::TypeMismatchError) { infer [:null, :fix] }
      end
      
      should "map" do
        assert_equal [[:a, :b], [[:list, :of, :a], [:list, :of, :b]]],
          infer([:fix,
            [:^, :rec,
              [:^, :f,
                [:^, :xs,
                  [:ifelse, [:null, :xs],
                    :nil,
                    [:cons, [:f, [:car, :xs]],
                            [:rec, :f, [:cdr, :xs]]]]]]]])

        assert_equal [[:a, :a], [[:list, :of, :a], [:list, :of, :a]]],
          infer([:fix,
            [:^, :rec,
              [:^, :f,
                [:^, :xs,
                  [:ifelse, [:null, :xs],
                    :xs,
                    [:cons, [:f, [:car, :xs]],
                            [:rec, :f, [:cdr, :xs]]]]]]]])
      end
      
      should "filter" do
        assert_equal [[:a, :bool], [[:list, :of, :a], [:list, :of, :a]]],
          infer([:fix,
            [:^, :rec,
              [:^, :pred,
                [:^, :xs,
                  [:ifelse, [:null, :xs],
                    :nil,
                    [:ifelse, [:pred, [:car, :xs]],
                      [:cons, [:car, :xs],
                              [:rec, :pred, [:cdr, :xs]]],
                      [:rec, :pred, [:cdr, :xs]]]]]]]])
      end
      
      should "foldl" do
        assert_equal [[:a, [:b, :a]], [:a, [[:list, :of, :b], :a]]],
          infer([:fix,
            [:^, :rec,
              [:^, :f,
                [:^, :z,
                  [:^, :xs,
                    [:ifelse, [:null, :xs],
                      :z,
                      [:rec, :f, [:f, :z, [:car, :xs]],
                                 [:cdr, :xs]]]]]]]])
      end
      
      should "foldr" do
        assert_equal [[:a, [:b, :b]], [:b, [[:list, :of, :a], :b]]],
          infer([:fix,
            [:^, :rec,
              [:^, :f,
                [:^, :z,
                  [:^, :xs,
                    [:ifelse, [:null, :xs],
                      :z,
                      [:f, [:car, :xs],
                           [:rec, :f, :z, [:cdr, :xs]]]]]]]]])
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
    end
  end
end
