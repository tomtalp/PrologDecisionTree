del(X, [X | Rest], Rest):- !.
del(X, [Y | Rest0], [Y | Rest]):-
    del(X, Rest0, Rest).

attribute(size, [small, large]).
attribute(shape, [long, compact, other]).
attribute(holes, [none, 1, 2, 3, many]).
/*
example(nut, [size = small]).
example(nut, [size = small]).
example(nut, [size = small]).
example(nut, [size = small]).
example(nut, [size = large]).
example(nut, [size = small]).
example(nut, [size = small]).
example(nut, [size = large]).
example(nut, [size = large]).
example(nut, [size = large]).
example(nut, [size = large]).
example(nut, [size = small]).
*/
example(nut, [size = small, shape = compact, holes = 1]).
example(screw, [size = small, shape = long, holes = none]).
example(key, [size = small, shape = long, holes = 1]).
example(nut, [size = small, shape = compact, holes = 1]).
example(key, [size = large, shape = long, holes = 1]).
example(screw, [size = small, shape = compact, holes = none]).
example(nut, [size = small, shape = compact, holes = 1]).
example(pen, [size = large, shape = long, holes = none]).
example(scissors, [size = large, shape = long, holes = 2]).
example(pen, [size = large, shape = long, holes = none]).
example(scissors, [size = large, shape = long, holes = 2]).
example(key, [size = small, shape = other, holes = 2]).

induce_tree(Tree):-
    findall(example(Class, Obj), example(Class, Obj), Examples),
    findall(Att, attribute(Att, _), Attributes),
    induce_tree(Attributes, Examples, Tree),!,write(Tree).

induce_tree(_, [], null):- !. % No examples to learn from
induce_tree([], _, null):- !. % No attributes left

induce_tree(_, [example(Class, _) | Examples], leaf(Class)):- % Only one class in our dataset
    write("Only 1 class!"),nl,
    \+ (
        member(example(Class2, _), Examples),
        Class2 \== Class
    ), !.

induce_tree(Attributes, Examples, tree(Attribute, SubTrees)):-
    write('HOLA'),nl,
    choose_attribute(Attributes, Examples, Attribute), % Select the best attribute
    write("Selected attribute for split = "), write(Attribute), nl,
    del(Attribute, Attributes, RemainingAttributes),
    attribute(Attribute, Values), % Get the possible values of the selected attribute
    induce_trees(Attribute, Values, RemainingAttributes, Examples, SubTrees),
    write("Finished first iteration"),nl.
    % write(RemainingAttributes),nl,
    % write(Examples).

%induce_trees(Att, Vals, RestAtts, Examples, SubTrees)

induce_trees(_, [], _, _, []).


induce_trees(Att, [Val1 | Vals], RestAtts, Examples, [Val1 : Tree1 | Trees]):-
    write("Inside induce_trees! Val1 = "), write(Val1), write(" remaining vals = "), write(Vals), write(" RestAtts = "), write(RestAtts), nl,
    get_subset_by_feature_query(Att = Val1, Examples, ExampleSubset),
    write(" RestAtts = "), write(RestAtts), write(" ExampleSubset = "), write(ExampleSubset), write("Tree1 = "), write(Tree1), nl,
    induce_tree(RestAtts, ExampleSubset, Tree1),
    write("Tree1 = "), write(Tree1), nl,
    write("After call to `induce_tree` which apparently doesn't work"),nl,
    induce_trees(Att, Vals, RestAtts, Examples, Trees).

satisfy(Object, Conj):-
    \+ (
        member(Att = Val, Conj),
        member(Att = ValX, Object),
        ValX \== Val
    ).

get_subset_by_feature_query(Feature = Value, Examples, ExampleSubset):-
    findall(
        example(Class, Obj), 
        (
            member(example(Class, Obj), Examples),
            satisfy(Obj, [Feature = Value])
        ),
        ExampleSubset
    ).        


% impurity1(Examples, Attribute, Impurity):-

/*
    get_squared_sum_of_freqs(FrequenciesList, FrequenciesSum, SquaredSum)

    Receive a list representing frequencies, the some of all frequencies, and return
    the squared sum of the probabilities. This will be subtracted from 1 to calc the Gini impurity metric
    Sum equation = (Freq_0 / FrequenciesSum)^2 + (Freq_1 / FrequenciesSum)^2 + ... + (Freq_n / FrequenciesSum)^2

    e.g. - 
        FrequenciesList = [ClassA/3, ClassB/2, ClassC/2],
        FrequenciesSum = 7,
        SquaredSum ---> (3/7)**2 + (2/7)**2 + (2/7)**2 ---> 0.3469
*/
get_squared_sum_of_freqs([], _, 0).
get_squared_sum_of_freqs([_/Freq | T], FrequenciesSum, Sum):-
    N is (Freq / FrequenciesSum) ** 2,
    get_squared_sum_of_freqs(T, FrequenciesSum, Sum1),
    Sum is Sum1 + N.

/*
    get_gini_impurity(Examples, FeatureName, FeatureValue, GiniImpurity)

    Calculate the Gini impurity value for a given feature split.
    We take all the training data with FeatureName = FeatureValue, calculate the frequencies
    of each class, then subtract the squared sum of the class probabilities from 1. 
    A more in depth explanation can be found in the project docs, page <TODO>

    e.g. - 
        Examples = [example(ClassA, _), example(ClassA, _), example(ClassB, _), ......],
        FeatureName = dummy_feature,
        FeatureValue = Blabla,
        GiniImpurity ---> 0.6531

    Running test - 
    findall(example(Class, Obj), example(Class, Obj), Examples), get_gini_impurity(Examples, size, small, GiniImpurity).
*/
get_gini_impurity(ExamplesSubset, SubsetLength, GiniImpurity):-
    count_freqs(ExamplesSubset, Frequencies),
    get_squared_sum_of_freqs(Frequencies, SubsetLength, Sum),
    GiniImpurity is 1 - Sum.

/*
    get_gini_impurity_for_feature(TrainingSet, FeatureName, FeatureGiniImpurity)

    Calculate the Gini impurity for an entire feature, by performing a weighted sum of the Gini impurity values 
    of every feature value for the given feature

    e.g. - 
        TrainingSet = [example(ClassA, _), example(ClassA, _), example(ClassB, _), ......],
        FeatureName = test_feature,
        FeatureGiniImpurity ---> 0.62516


    findall(example(Class, Obj), example(Class, Obj), TrainingSet), get_gini_impurity_for_feature(TrainingSet, size, Gini)
*/

get_gini_impurity_for_feature(TrainingSet, FeatureName, FeatureGiniImpurity):-
    attribute(FeatureName, FeatureValues),
    length(TrainingSet, TrainingSetSize),
    get_gini_impurities(TrainingSet, TrainingSetSize, FeatureName, FeatureValues, FeatureGiniImpurity).

/*
    get_gini_impurities(TrainingSet, TrainingSetSize, FeatureName, FeatureValues, ImpurityWeightedSum)

    Perform a sum of the Gini impurity values, for every possible value in our feature-value list.
    We go over all the values for the given feature, extract the training dataset with that value and
    calculate the Gini impurity for that single value. Then we perform a weighted sum for all impurities.
    Further explanation in the project document, page <TODO>
*/
get_gini_impurities(_, _, _, [], 0):- !.

get_gini_impurities(TrainingSet, TrainingSetSize, FeatureName, [FeatureValue | FeatureValueList], ImpurityWeightedSum):-
    /* Extract all training examples with the given value */
    get_subset_by_feature_query(FeatureName = FeatureValue, TrainingSet, TrainingSubset),
    
    /* Calculate the Gini impurity for this training set */
    length(TrainingSubset, SubsetSize),
    get_gini_impurity(TrainingSubset, SubsetSize, GiniImpurity),

    /* Recursively get the sum for all other feature values */
    get_gini_impurities(TrainingSet, TrainingSetSize, FeatureName, FeatureValueList, ImpurityWeightedSum1),

    /* Add up the Gini values according to their weight in the entire dataset */
    ImpurityWeightedSum is ImpurityWeightedSum1 + ((SubsetSize / TrainingSetSize) * GiniImpurity).
    

/*  
    count_freqs(Examples, Frequencies)
    Count frequencies for every class in a given example subset
    e.g. -
        Examples = [example(ClassA, _), example(ClassA, _), example(ClassB, _)],
        Frequencies ---> [ClassA/2, ClassB/1]
*/
count_freqs([], Res):-
    findall(Class/Freq, retract(freq(Class, Freq)), Res), !.

count_freqs([example(Class, _) | T], Res):-
    (
        (
            retract(freq(Class, N)),!,
            N1 is N+1,
            assert(freq(Class, N1))
        )
        ;
        assert(freq(Class, 1))
    ),
    count_freqs(T, Res).



choose_attribute(Atts, Examples, BestAtt):-
    setof(
        Impurity/Att,
        (
            member(Att, Atts), get_gini_impurity_for_feature(Examples, Att, Impurity)
        ),
        [MinImpurity/BestAtt | _]
    ).


% Testing
%findall(example(Class, Obj), example(Class, Obj), Examples),
%findall(Att, attribute(Att, _), Attributes),
%get_subset_by_feature_query(size = small, Examples, R).