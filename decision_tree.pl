del(X, [X | Rest], Rest):- !.
del(X, [Y | Rest0], [Y | Rest]):-
    del(X, Rest0, Rest).

satisfy(Object, Conj):-
    \+ (member(Att = Val, Conj),
        member(Att = ValX, Object),
        ValX \== Val
    ).

attribute(size, [small, large]).
attribute(shape, [long, compact, other]).
attribute(holes, [none, 1, 2, 3, many]).

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
    induce_tree(Attributes, Examples, Tree).

induce_tree(_, [], null):- !. % No examples to learn from

induce_tree(_, example(Class, _), leaf(Class)):- % Only one class in our dataset
    \+ (member(example(Class2, _), Examples)),
    Class2 \== Class, !.
induce_tree(Attributes, Examples, tree(Attribute, SubTrees)):-
    choose_attribute(Attributes, Examples, Attribute), % Select the best attribute
    del(Attribute, Attributes, RemainingAttributes),
    attribute(Attribute, Values), % Get the possible values of the selected attribute
    induce_trees(Attribute, Values, RemainingAttributes, Examples, SubTrees).

%induce_trees(Att, Vals, RestAtts, Examples, SubTrees)
induce_trees(_, [], _, _, []).

/*
induce_trees(Att, [Val1 | Vals], RestAtts, Examples, [Val1 : Tree1 | Trees]):-
    attval_subset(Att = Val1, Examples, ExampleSubset),
*/
attval_subset(Attribute = Value, Examples, ExampleSubset):-
    findall(
        example(Class, Obj), 
        (
            member(example(Class, Obj), Examples),
            satisfy(Obj, [Attribute = Value])
        ),
        ExampleSubset
    ).        


% impurity1(Examples, Attribute, Impurity):-

% findall(example(Class, Obj), example(Class, Obj), Examples), get_probas(Examples, size, small, P).
get_probas(Examples, Attribute, AttrValue, Probas):-
    attval_subset(Attribute = AttrValue, Examples, Subset),
    write('subset = '),nl, write(Subset), nl,
    
    length(Subset, SubsetSize),
    write('SubsetSize = '),nl, write(SubsetSize), nl,

    length(Examples, ExamplesSize),
    write('ExamplesSize = '),nl, write(ExamplesSize), nl,
    count_freqs(Subset, Probas).




count_freqs([], Res):-
    findall(Class/Freq, retract(freq(Class, Freq)), Res), !.

count_freqs([example(Class, _) | T], Res):-
    length(T, TSize),
    write('Inside count_freqs. Class = '), write(Class), writef(' TSize = %d', TSize), nl,
    (
        (
            retract(freq(Class, N)),
            N1 is N+1,
            assert(freq(Class, N1))
        )
        ;
        assert(freq(Class, 1))
    ),!,
    count_freqs(T, Res).





choose_attribute(Atts, Examples, BestAtt):-
    setof(
        Impurity/Att,
        (
            member(Att, Atts), impurity1(Examples, Att, Impurity)
        ),
        [MinImpurity/BestAtt | _]
    ).

foo([X | T], X).

% Testing
%findall(example(Class, Obj), example(Class, Obj), Examples),
%findall(Att, attribute(Att, _), Attributes),
%attval_subset(size = small, Examples, R).