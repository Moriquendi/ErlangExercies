 -module(addressBook).

-export([createAddressBook/0,
		addContact/3,
		addEmail/4,
		addPhone/4,
		removeContact/3,
		removeEmail/2,
		removePhone/2,
		getEmails/3,
		getPhones/3,
		findByEmail/2,
		findByPhone/2,
		addNote/5,
		searchForNote/4]).


% Książka powinna przechowywać:
% 	imiona,
% 	nazwiska,
% 	numery telefonów,
% 	adresy e-mail.

-record(person, {firstName,
				lastName,
				phone,
				email,
				notes=[]}).

-record(book, {people=[]}).

% createAddressBook/0 - tworzy i zwraca nową książkę adresową;
createAddressBook() ->
	#book{people=[]}.

% addContact/3 - dodaje wpis: (imię, nazwisko); zwraca zaktualizowaną książkę;
addContact(Book, First, Last) ->
	NewPerson = #person{firstName=First, lastName=Last},
	NewPeople = Book#book.people ++ [NewPerson],
	Book#book{people = NewPeople}.

% addEmail/4 - dodaje lub aktualizuje wpis: (imię, nazwisko, email);
% zwraca zaktualizowaną książkę;

findAndReplace([H | T], Person, NewPerson) ->
	First = Person#person.firstName,
	Last = Person#person.lastName,

	IsMatch = (H#person.firstName ==  First) and (H#person.lastName == Last),
	if
		 IsMatch == true->
			[NewPerson | T];
		true ->
			[H] ++ findAndReplace(T, Person, NewPerson)
	end.

matchPerson(Book, First, Last) ->
	lists:filter(fun(X) -> (X#person.firstName == First) and
							(X#person.lastName == Last) end,
							Book#book.people).

matchPersonWithEmail(Book, Email) ->
	lists:filter(fun(X) -> (X#person.email == Email) end,
							Book#book.people).
matchPersonWithPhone(Book, Phone) ->
	lists:filter(fun(X) -> (X#person.phone == Phone) end,
							Book#book.people).

updateContact(Book, OldContact,  NewContact) ->	
	% Stworz liste ludzi z nowym kontaktem
	NewPeople = findAndReplace(Book#book.people, OldContact, NewContact),
	% Edytuj ostatecznie ksiazke i zwroc
	Book#book{people = NewPeople}.

matchOrCreateContactIfNotExist(Book, First, Last) ->
	%	Sprawdz czy kontakt jest w ksiazce. 
	MatchList = matchPerson(Book, First, Last),

	% Jesli nie to go utworz
	if
		MatchList == [] ->
			NewBook = addContact(Book, First, Last),
			[Match] = matchPerson(NewBook, First, Last);
		true-> 
			NewBook = Book,
			[Match] = MatchList
	end,

	{NewBook, Match}.

addEmail(Book, First, Last, Email) ->
	% Check if object is in address book and take it. If it doesnt exist, create it.
	{NewBook, Match} = matchOrCreateContactIfNotExist(Book, First, Last),
	% Stowrz edytowana kopie
	UpdatedMatch = Match#person{email = Email},
	% Update and return new book
	updateContact(NewBook, Match, UpdatedMatch).

	
% addPhone/4 - dodaje lub aktualizuje wpis: (imię, nazwisko, telefon); zwraca zaktualizowaną książkę;
addPhone(Book, First, Last, Phone) ->
	{NewBook, Match} = matchOrCreateContactIfNotExist(Book, First, Last),
	UpdatedMatch = Match#person{phone = Phone},
	updateContact(NewBook, Match, UpdatedMatch).

% removeContact/3 - usuwa wpis dla danego imienia i nazwiska, zwraca zaktualizowaną książkę,
removeContact(Book, First, Last) ->
	NewPeople = lists:filter(fun(X) -> not ((X#person.firstName == First) and
											(X#person.lastName == Last)) end,
										Book#book.people),
	Book#book{people = NewPeople}.

% removeEmail/2 - usuwa podany email, zwraca zaktualizowaną książkę;
removeEmail(Book, Email) ->
	MatchList = matchPersonWithEmail(Book, Email),
	if
		MatchList == [] -> Book;
		true -> 
			[Match] = MatchList,
			% Stowrz edytowana kopie
			UpdatedMatch = Match#person{email = ""},
			% Update and return new book
			updateContact(Book, Match, UpdatedMatch)
	end.
	
% removePhone/2 - usuwa podany telefon, zwraca zaktualizowaną książkę;
removePhone(Book, Phone) ->
	MatchList = matchPersonWithPhone(Book, Phone),
	if
		MatchList == [] -> Book;
		true -> 
			[Match] = MatchList,
			UpdatedMatch = Match#person{phone = ""},
			updateContact(Book, Match, UpdatedMatch)
	end.

% getEmails/3 - zwraca listę adresów email dla danego imienia i nazwiska;
getEmails(Book, First, Last) ->
	MatchList = matchPerson(Book, First, Last),
	if
		MatchList == [] -> [];
		true -> 
			[Match] = MatchList,
			Match#person.email
	end.

% getPhones/3 - zwraca listę numerów telefonu dla danego imienia i nazwiska;
getPhones(Book, First, Last) ->
	MatchList = matchPerson(Book, First, Last),
	if
		MatchList == [] -> [];
		true -> 
			[Match] = MatchList,
			Match#person.phone
	end.

% findByEmail/2 - zwraca imię i nazwisko dla danego adresu email;
findByEmail(Book, Email) ->
	MatchList = matchPersonWithEmail(Book, Email),
	if
		MatchList == [] -> [];
		true -> 
			[Match] = MatchList,
			[Match#person.firstName, Match#person.lastName]
	end.

% findByPhone/2 - zwraca imię i nazwisko dla danego numeru telefonu;
findByPhone(Book, Phone) ->
	MatchList = matchPersonWithPhone(Book, Phone),
	if
		MatchList == [] -> [];
		true -> 
			[Match] = MatchList,
			[Match#person.firstName, Match#person.lastName]
	end.

% Dodaj mozliwosc przechowywania notatek (rodzaj, tresc).
addNote(Book, First, Last, Type, Note) ->
	MatchList = matchPerson(Book, First, Last),
	if
		MatchList == [] -> [];
		true ->
			[Match] = MatchList,
			UpdatedMatch = Match#person{notes = Match#person.notes ++ [{Type, Note}]},
			updateContact(Book, Match, UpdatedMatch)			
	end.

% Dodaj mozliwosc wyszukiwania po fragmentach notatek. 
searchForNote(Book, First, Last, NoteFragment) ->
	MatchList = matchPerson(Book, First, Last),
	if
		MatchList == [] -> [];
		true ->
			[Match] = MatchList,
			Notes = Match#person.notes,
			findNote(Notes, NoteFragment)
	end.

findNote([], _) -> [];
findNote([H | T], NoteFragment) ->
	{_, Note} = H,
	Pos = string:str(Note, NoteFragment),
	if
		Pos == 0 -> findNote(T, NoteFragment);
		true -> Note
	end.
