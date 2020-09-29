offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1),
bus).
offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31),
10, 1), cabin).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding,
50).
customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel,
100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin,
79).

% ------------------------------------------------------------------------

possibleSubset([],[]).
possibleSubset(List,A):-
                        subset1(List,R),
                        permutation(R,A).
subset1([], []).
subset1([E|Tail], [E|NTail]):-
  subset1(Tail, NTail).
subset1([_|Tail], NTail):-
  subset1(Tail, NTail).

% ------------------------------------------------------------------------

choosePreferences(Prefs, ChosenPreferences):-
    getActivityList(Prefs, X),
    subset1(X,R),
    setActivityList(Prefs,R,Prefs1),
    subset1(Prefs1,ChosenPreferences).
getActivityList([],[]).
getActivityList([activity(R)|_],R).
getActivityList([H|T],R):-
                        H \= activity(_),
                        getActivityList(T,R).
setActivityList([],_,[]).
setActivityList([activity(_)|T],X,[activity(X)|T1]):-
                        setActivityList(T,X,T1).
setActivityList([H|T],X,[H|T1]):-
                        H \= activity(_),
                        setActivityList(T,X,T1).

% ------------------------------------------------------------------------

preferenceSatisfaction(Offer,Customer,ChosenPrefs,S):-
    helper(Offer,Customer,ChosenPrefs,S,0).
helper(_,_,[],Acc,Acc).
helper(Offer, Customer, [accommodation(A)|T],S,Acc):-
     offerAccommodation(Offer,A),
     customerPreferredAccommodation(Customer,A,R1),
     Acc1 is Acc+R1,
     helper(Offer, Customer, T,S,Acc1).
helper(Offer, Customer, [means(M)|T],S,Acc):-
    offerMean(Offer,M),
    customerPreferredMean(Customer,M,R2),
    Acc1 is Acc+R2,
    helper(Offer, Customer, T,S,Acc1).
helper(Offer, Customer, [activity([])|T],S,Acc):-
        helper(Offer, Customer, T,S,Acc).
helper(Offer, Customer, [activity(X)|T],S,Acc):-
    Offer = offer(_,A,_,_,_,_,_,_),
    checkActivity(A,X),
    X=[H|R],
    customerPreferredActivity(Customer,H,R3),
    Acc1 is Acc+R3,
    helper(Offer, Customer, [activity(R)|T],S,Acc1).
helper(Offer, Customer, [activity(X)|T],S,Acc):-
    Offer = offer(_,A,_,_,_,_,_,_),
    not(checkActivity(A,X)),
    helper(Offer, Customer, T,S,Acc).
helper(Offer, Customer, [H|T],S,Acc):-
     H \= accommodation(_),
     H \= means(_),
     H \= activity(_),
     helper(Offer, Customer, T,S,Acc).
checkActivity(L,A):-
                        possibleSubset(L,A);
                        possibleSubset(A,L).

% ------------------------------------------------------------------------

overlapPeriod(P1,P2):-
      P1 = period(D1,D2),
      P2 = period(D3,_),
      D3 @=< D2,
      D3 @>= D1.
overlapPeriod(P1,P2):-
      P1 = period(D1,D2),
      P2 = period(D3,D4),
      D3 @=< D2,
      D3 @=< D1,
      D4 @>= D1,
      D4 @=< D2.

% ------------------------------------------------------------------------

getOffer([],_).
getOffer(ChosenPrefs,Offer):-
                      ChosenPrefs=[dest(D)|T],
                      offerMean(Offer,_),
                      Offer=offer(D,_,_,_,_,_,_,_),
                      getOffer(T,Offer).
getOffer(ChosenPrefs,Offer):-
                        ChosenPrefs=[budget(B)|T],
                        offerMean(Offer,_),
                        Offer=offer(_,_,C,_,_,_,_,_),
                        C =< B,
                        getOffer(T,Offer).
getOffer(ChosenPrefs,Offer):-
                        ChosenPrefs=[activity(A)|T],
                        A \= [],
                        offerMean(Offer,_),
                        Offer=offer(_,X,_,_,_,_,_,_),
                        possibleSubset(X,A),
                        getOffer(T,Offer).
getOffer(ChosenPrefs,Offer):-
                        ChosenPrefs=[H|T],
                        H=period(_,_),
                        offerMean(Offer,_),
                        Offer=offer(_,_,_,_,_,P,_,_),
                        overlapPeriod(H,P),
                        getOffer(T,Offer).
getOffer(ChosenPrefs,Offer):-
                        ChosenPrefs=[accommodation(A)|T],
                        offerAccommodation(Offer,A),
                        getOffer(T,Offer).
getOffer(ChosenPrefs,Offer):-
                        ChosenPrefs=[means(M)|T],
                        offerMean(Offer,M),
                        getOffer(T,Offer).
getOffer(ChosenPrefs,Offer):-
                        ChosenPrefs = [H|T],
                        H \= dest(_),
                        H \= budget(_),
                        H \= activity(_),
                        H \= period(_,_),
                        H \= accommodation(_),
                        H \= means(_),
                        getOffer(T,Offer).

% ------------------------------------------------------------------------

recommendOfferForCustomer(Prefs, ChosenPrefs, O):-
                        choosePreferences(Prefs,ChosenPrefs),
                        ChosenPrefs \= [],
                        getOffer(ChosenPrefs,O).

% ------------------------------------------------------------------------

recommendOffer([],[],_,_).
recommendOffer(Customers, PreferenceList, Offer, CustomersChosen):-
                        Customers=[_|T],
                        PreferenceList=[_|T1],
                        offerMean(Offer,_),
                        chosen(Offer,0,Customers,PreferenceList,CustomersChosen),
                        recommendOffer(T,T1,_,_).
chosen(_,_,[],[],[]).
chosen(Offer,Counter,  C,P,[]):-
        C\=[],
        P\=[],
        Offer=offer(_,_,_,_,_,_,_,N),
        Counter=N.
chosen(Offer,Counter,  Customers,PreferenceList,CustomersChosen):-
   Customer\=[],
   PreferenceList\=[],
   Offer=offer(_,_,_,_,_,_,_,N),
   Counter<N,
   highestSatisfaction(Offer, Customers,PreferenceList, 0, MaxCustomer, MaxCustPrefs),
   delete(Customer,MaxCustomer,Newcustomerslist),
   delete(PreferenceList, MaxCustPrefs, NewPreferenceList),
   CustomersChosen=[MaxCustomer|T],
   Newcounter is Counter+1,
   chosen(Offer, Newcounter, Newcustomerslist,NewPreferenceList,T).
highestSatisfaction(_,[],[],_,_,_).
highestSatisfaction(Offer,[H|T],[H1|T1],Temp,H,H1):-
                        preferenceSatisfaction(Offer,H,H1,S),
                        S > Temp,
                        highestSatisfaction(Offer,T,T1,S,H,H1).
highestSatisfaction(Offer,[H|T],[H1|T1],Temp,R,R1):-
                        preferenceSatisfaction(Offer,H,H1,S),
                        S =< Temp,
                        highestSatisfaction(Offer,T,T1,Temp,R,R1).











