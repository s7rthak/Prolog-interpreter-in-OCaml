male(surya).
male(yama).
male(vayu).
male(indra).
male(ashwini).

male(kuru).

male(shantanu).
male(parashara).

male(bhisma).
male(chitrangada).
male(vichitravirya).
male(vyasa).
male(subala).

male(dhritarashtra).
male(shakuni).
male(pandu).

male(karna).
male(yudhishthira).
male(bhima).
male(arjuna).
male(nakula).
male(sahadev).
male(duryodhana).
male(dushasana).
male(yuyutsu).

male(abhimanyu).
male(prativindhya).
male(sutasoma).
male(srutakirti).
male(satanika).
male(srutakarma).

male(parikshit).

male(janamejaya).

female(ganga).
female(satyavati).

female(ambika).
female(ambalika).

female(gandhari).
female(kunti).
female(madri).
female(vaishya).

female(dussala).
female(subhadra).
female(draupadi).

female(uttara).

female(madravati).

child(shantanu, kuru). 

child(bhisma, shantanu). 
child(bhisma, ganga).

child(chitrangada, shantanu).
child(chitrangada, satyavati).

child(vichitravirya, shantanu).
child(vichitravirya, satyavati).

child(vyasa, parashara).
child(vyasa, satyavati).

child(dhritarashtra, vichitravirya).
child(dhritarashtra, vyasa).
child(dhritarashtra, ambika).

child(pandu, vichitravirya). 
child(pandu, vyasa).
child(pandu, ambalika).

child(shakuni, subala). 

child(gandhari, subala). 

child(duryodhana, dhritarashtra). 
child(duryodhana, gandhari).

child(dussala, dhritarashtra). 
child(dussala, gandhari).

child(dushasana, dhritarashtra). 
child(dushasana, gandhari).

child(yuyutsu, dhritarashtra). 
child(yuyutsu, vaishya).

child(karna, kunti). 
child(karna, surya).

child(yudhishthira, pandu). 
child(yudhishthira, kunti).
child(yudhishthira, yama).

child(bhima, pandu). 
child(bhima, kunti).
child(bhima, vayu).

child(arjuna, pandu). 
child(arjuna, kunti).
child(arjuna, indra).

child(nakula, pandu). 
child(nakula, madri).
child(nakula, ashwini).

child(sahadev, pandu). 
child(sahadev, madri).
child(sahadev, ashwini).

child(abhimanyu, arjuna). 
child(abhimanyu, subhadra).

child(prativindhya, yudhishthira). 
child(prativindhya, draupadi).

child(sutasoma, bhima). 
child(sutasoma, draupadi).

child(srutakirti, arjuna). 
child(srutakirti, draupadi).

child(satanika, nakula). 
child(satanika, draupadi).

child(srutakarma, sahadev). 
child(srutakarma, draupadi).

child(parikshit, abhimanyu). 
child(parikshit, uttara).

child(janamejaya, parikshit). 
child(janamejaya, madravati).

married(shantanu, ganga).		
married(shantanu, satyavati).
married(parashara, satyavati).
married(vichitravirya, ambika).
married(vichitravirya, ambalika).
married(dhritarashtra, gandhari).
married(pandu, kunti).
married(pandu, madri).
married(arjuna, subhadra).
married(yudhishthira, draupadi).
married(bhima, draupadi).
married(arjuna, draupadi).
married(nakula, draupadi).
married(sahadev, draupadi).
married(abhimanyu, uttara).
married(parikshit, madravati).

is_married(A, B):-  married(A, B).
is_married(C, D):-  married(D, C).

husband(E, F) :-  male(E), married(E, F).
husband(G, H) :-  male(G), married(H, G).

wife(I, J)  :-  female(I), married(I, J).
wife(K, L):-  female(K), married(L, K).

grandfather(M, N):-	male(M), child(N, O), child(O, M).
grandmother(P, Q):-	female(P), child(Q, R), child(R, P).

descendent(S, T):-	child(S, T).
descendent(U, V):-	child(U, W), descendent(W, V).

ancestor(Y, Z):-	descendent(Z, Y).
