TITLE MASM Template						(main.asm)

INCLUDE Irvine32.inc
;Everything taken from the book or Irvine32.lib

.data
cardWidth	= 10																		; width of card = 10
blankcard  BYTE "|        |",0														; ASCII Art for cards
cardline1  BYTE ".--------.",0
cardline2  BYTE "|",0
cardline6  BYTE        "|",0
cardline7  BYTE "`--------'",0

; for hearts
cardline2a BYTE    "_  _  |",0
cardline3a BYTE "| ( \/ ) |",0
cardline4a BYTE "|  \  /  |",0
cardline5a BYTE "|   \/ ",0
	
;diamond
cardline2b BYTE    " /\   |",0
cardline3b BYTE "|  /  \  |",0
cardline4b BYTE "|  \  /  |",0
cardline5b BYTE "|   \/ ",0

;clubs
cardline2c BYTE    " _    |",0
cardline3c BYTE "|  ( )   |",0
cardline4c BYTE "| (_X_)  |",0
cardline5c BYTE "|   Y  ",0

;spades
cardline2d BYTE    " .    |",0
cardline3d BYTE "|  / \   |",0
cardline4d BYTE "| (_,_)  |",0
cardline5d BYTE "|   I  ",0

CARD STRUCT																		; Holds a card's Face, Suit, and value
	Face		WORD 00		; Card face (A, 2, ..., K)
	Suit		BYTE 0		; Card suit
	Value	BYTE 0		; Value of Card
CARD ENDS

cardDeck	CARD <' A',1,11>,	<' A',2,11>,	<' A',3,11>,	<' A',4,11>						; Will not allow me to create them all in one block so I have to separate 
		CARD	<' 2',1,2>,	<' 2',2,2>,	<' 2',3,2>,	<' 2',4,2>						; with CARD in front
		CARD	<' 3',1,3>,	<' 3',2,3>,	<' 3',3,3>,	<' 3',4,3>
		CARD	<' 4',1,4>,	<' 4',2,4>,	<' 4',3,4>,	<' 4',4,4>
		CARD	<' 5',1,5>,	<' 5',2,5>,	<' 5',3,5>,	<' 5',4,5>
		CARD	<' 6',1,6>,	<' 6',2,6>,	<' 6',3,6>,	<' 6',4,6>
		CARD	<' 7',1,7>,	<' 7',2,7>,	<' 7',3,7>,	<' 7',4,7>
		CARD	<' 8',1,8>,	<' 8',2,8>,	<' 8',3,8>,	<' 8',4,8>
		CARD	<' 9',1,9>,	<' 9',2,9>,	<' 9',3,9>,	<' 9',4,9>
		CARD	<'10',1,10>,	<'10',2,10>,	<'10',3,10>,	<'10',4,10>
		CARD	<' J',1,10>,	<' J',2,10>,	<' J',3,10>,	<' J',4,10>
		CARD	<' Q',1,10>,	<' Q',2,10>,	<' Q',3,10>,	<' Q',4,10>
		CARD	<' K',1,10>,	<' K',2,10>,	<' K',3,10>,	<' K',4,10>

deckAsNum BYTE 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25				; instead of shuffling the cardDeck, just shuffle this instead
		BYTE 26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51		; use the numbers in this array to access the corresponding card in cardDeck

; both dealer and player can only hold up to 5 cards, after which they will automatically stand
dealerHand CARD 10 DUP(<>)															; Dealer hand
playerHand CARD 10 DUP(<>)															; Player Hand
dealerValue BYTE 0																	; Sum of the value of cards in dealer hand
playerValue BYTE 0																	; Sum of the value of cards in player hand
player BYTE 1																		; These two will be used for invoking procs
dealer BYTE 0
dealerHandMsg BYTE "Dealer's Hand: ",0													; indicate which hand belongs to who on display
playerHandMsg BYTE "Player's Hand: ",0

deckSize = 52																		; number of cards in a deck
numberOfDecks BYTE ?																; number of decks user will want to play with

playerTurn BYTE 1																	; used as a boolean to check if its the player's turn or not
																				; leaves the dealer's first card covered and only shows second one
playerCash SDWORD 500																; starting playerCash
playerBet	 DWORD 50																	; player bet
movePrompt BYTE "0 - Stay, 1 - Hit, 2 - Fold",0											; prompt for user's choice (stay, hit)
playerAction DWORD ?																; stores player's action
noCashPrompt BYTE "Out of Cash!", 0													; no more cash message
gamePrompt BYTE "0 - Play Again?, 1 - Quit?",0											; prompt user to play again or quit
cashPrompt BYTE "How much money would you like to start with? ",0							; prompt for starting cash amount
betPrompt  BYTE "How much would you like to bet? ",0										; prompt for bet amount
welcomeMessage BYTE "Welcome to a game of BlackJack!", 0									; welcome message
goodbyeMsg BYTE "Thank you for playing. Good Bye!",0										; exit message
betMsg BYTE "Bet: ",0																; used to show current bet
cashMsg BYTE "Remaining Cash: ",0														; used to show current cash remaining for the player

playerWinsMsg	BYTE "Player Wins!",0													; victory and lose messages
dealerWinsMsg	BYTE "Dealer Wins!",0
bustMsg		BYTE "BUST! Dealer Wins!",0
blackJackMsg	BYTE "BlackJack!",0
pushMsg		BYTE "Push! Bet is returned.",0											; When dealer successfully pushes, bet is returned
dealerToPush	BYTE "Dealer will now try to push.",0

Hit PROTO,																		; prototypes for Hit, PrintHand, and checkForAce
	turn:BYTE																		; for all three procs, turn refers to player or dealer
PrintHand PROTO,																	; 1 = player, 0 = dealer
	turn:BYTE
checkForAce PROTO,
	turn:BYTE
															
.code
main PROC

	call Randomize																	; seed random number, taken from irvine32.lib
restart:																			; restart label used when player is out of cash and wants to play again
	call welcomePhase																; display welcome message and asks for starting cash amount

Game:																			; Game label is looped as long as player still has cash
	mov playerTurn, 1
	call Clrscr
	.IF (playerCash <= 0)															; when player has 0 cash
		mov dh, 10																
		mov dl, 20
		call Gotoxy																; Gottoxy is from irvine.lib, relocates cursor to (x,y) at (dh, dl)
		mov edx, OFFSET noCashPrompt
		call WriteString															; WriteString is from irvine32.lib, prints the null-terminated string pointed by edx
		mov dh, 11
		mov dl, 20
		call Gotoxy
		mov edx, OFFSET gamePrompt													; ask if player wants to play again or quit
		call WriteString															
		mov dh, 12
		mov dl, 20
		call Gotoxy
		call ReadDec																; ReadDec from irvine32.lib, reads in user input and stores number in eax
		.IF (eax == 0)																; Play again						
			jmp restart
		.ELSE																	; Quit
			jmp quitout
		.ENDIF
	.ENDIF

	mov ebx, OFFSET dealerHand														; point to dealer hand, hands will increment after each card given
	mov edx, OFFSET playerHand														; point to player hand
	mov esi, OFFSET deckAsNum														; point to numbers, will be using this to keep track of which cards have been drawn
																				; DealCards and Hit will increment esi each time a card is taken.
	call Shuffle																	; Shuffles cards each round
	call DealCards																	; deal out cards to dealer and player
	.IF(dealerValue == 21) || (playerValue == 21)										; when either has blackjack, it will start a new round
		jmp Game
	.ENDIF

	call betPhase																	; go into betPhase (asks user for their bet)
	call movePhase																	; go to movephase (asks user for their move)
		
	Invoke PrintHand, player															; print out both hands
	Invoke PrintHand, dealer
	call ClearHands																; clears both hands after each round
	jmp Game																		; starts a new round

	
quitout:																			; display goodbyemsg and exits program
	call Clrscr
	mov dh, 10																	
	mov dl, 20
	call Gotoxy
	mov edx, OFFSET goodbyeMsg
	call WriteString
	mov dh, 11
	mov dl, 20
	call Gotoxy
	mov eax, 1000																	; Delay for 1 sec before exitting so players can read the message
	call Delay

	exit
main ENDP


;---------------------------------------------------------------------------------------------------
welcomePhase PROC
;	Display welcomeMessage and cashPrompt, then reads in player's starting cash and saves to playerCash
;---------------------------------------------------------------------------------------------------
.code
	push edx																		; edx used for gotoxy and writestring
	push eax																		; eax used to store user input

	mov dh, 10																	; move to center of screen
	mov dl, 20
	call Gotoxy																	; move cursor to center
	mov edx, OFFSET welcomeMessage													; displays welcome prompt
	call WriteString
	mov dh, 11
	mov dl, 20
	call Gotoxy																	; skip a line
	mov edx, OFFSET cashPrompt														; asks for starting cash amount
	call WriteString
	mov dh, 12
	mov dl, 20
	call Gotoxy																	; skip a line
	call ReadDec																	; read in player input
	mov playerCash, eax																; saves player's choice into playerCash
	mov dh, 13
	mov dl, 20
	call Gotoxy																	; skip a line
	call WaitMsg																	; taken from irvine32.lib, displays a wait message and waits for user to press any key															
	call Clrscr

	pop eax
	pop edx
	ret
welcomePhase ENDP

;-------------------------------------------------------------------------------------------------
betPhase PROC
;	Display betPrompt and records player's bet
;-------------------------------------------------------------------------------------------------
.code
	push eax																		; eax used for reading input
	push edx																		; edx used for gotoxy and writestring

	mov dh, 10
	mov dl, 20
	call Gotoxy
	mov edx, OFFSET cashMsg															; Displays how much cash the player has remaining
	call WriteString
	mov eax, playerCash																; displays player's remaining cash
	call WriteInt
	mov dh, 11
	mov dl, 20
	call Gotoxy
	mov edx, OFFSET betPrompt														; Ask player how much they want to bet
	call WriteString
	call ReadDec																	; Reads in player's bet
	.IF (eax > playerCash)															; If player enters a bet amount higher than remaining cash, defaults bet to 
		mov eax, playerCash															; their remaining cash amount
	.ENDIF
	mov playerBet, eax																; saves bet into playerBet
	mov dh, 12
	mov dl, 20
	call Gotoxy
	call WaitMsg																	; wait message
	call Clrscr

	pop edx
	pop eax
	ret
betPhase ENDP

;-------------------------------------------------------------------------------------------------
movePhase PROC
;	Ask player for their action, hit, stay, quit
;	movePrompt BYTE "0 - Stay, 1 - Hit, 2 - Fold",0
;-------------------------------------------------------------------------------------------------
.code
	push eax																		; eax used to read in user's action and to modify playerCash with playerBet
	push edx																		; gotoxy and writestring
checkagain:
	Invoke PrintHand, player
	Invoke PrintHand, dealer
	.IF (playerValue == 21)									; will call dealerTurn to see if it can push (match player value so bet is not won or lost)
		mov dh, 10
		mov dl, 20
		call Gotoxy
		mov edx, OFFSET dealerToPush							; display that dealer will try to push
		call WriteString
		mov dh, 11
		mov dl, 20
		call Gotoxy
		call WaitMsg
		call dealerTurn									; Ends player turn and moves onto dealer's turn
		jmp endMove										; jumps to the end after
	.ELSEIF (playerValue > 21)
		INVOKE checkForAce, 1								; check if player has an ace
		.IF(playerValue <= 21)								; if there was an ace, it will jump back to checkagain, allowing player to take another action
			jmp checkagain
		.ENDIF
		mov dh, 10
		mov dl, 20
		call Gotoxy
		mov edx, OFFSET bustMsg								; if playerValue is over 21 and no more aces to convert, player busts
		call WriteString
		mov dh, 11
		mov dl, 20
		call Gotoxy
		mov eax, playerBet									; move playerBet to eax so that it can be subtracted from playerCash
		sub playerCash, eax
		call WaitMsg
		mov playerTurn, 0									; change playerTurn to 0 so it will show all of dealer's cards
		jmp endMove
	.ENDIF

	mov dh, 10
	mov dl, 20
	call Gotoxy
	mov edx, OFFSET betMsg									; Shows how much the player bet on this round
	call WriteString
	mov eax, playerBet
	call WriteDec
	mov dh, 11
	mov dl, 20
	call Gotoxy
	mov edx, OFFSET movePrompt								; Asks user for action: stay, hit, fold
	call WriteString
	mov dh, 12
	mov dl, 20
	call Gotoxy
	call ReadDec											; reads in user's action
	mov playerAction, eax
	call Clrscr

	.IF (eax == 0)											; Stand, goes to dealer's turn
		call dealerTurn								
	.ELSEIF (eax == 1)										; Hit, calls Hit for player and then jumps to checkAgain to determine if further action can be taken
		pop edx
		Invoke Hit, player
		push edx
		jmp checkAgain
	.ELSE												; Fold, player forfeits and loses bet. Moves onto next round
		mov eax, playerBet	
		sub playerCash, eax
	.ENDIF
endMove:													; When the player has either won or lost, it jumps here 
	pop edx
	pop eax
	ret
movePhase ENDP

;--------------------------------------------------------------------------------------------------
addCards PROC,
;	Re-adds player and dealer values after when checkForAce is called
;--------------------------------------------------------------------------------------------------
.code
	push esi												; points to playerHand
	push edi												; points to dealerHand
	push ecx												; used for loop
	push eax												; using al to sum up card values for player
	push ebx												; using bl to sum up card values for dealer

	mov playerValue, 0										; set both values to zero to re-add
	mov dealerValue, 0
	mov eax, 0											; eax and ebx used for summing cards
	mov ebx, 0

	mov esi, OFFSET playerHand								; pointers for playerHand and dealerHand
	mov edi, OFFSET dealerHand

	mov ecx, 10											; 10 cards a hand
readd:
	add al, (CARD PTR[esi]).Value								; add player cards into al
	add bl, (CARD PTR[edi]).Value								; add dealer cards into bl
	.IF (al == 0) && (bl == 0)
		jmp doneAdding										; all cards have been added up
	.ENDIF
	add esi, TYPE CARD										; increment esi and edi to move onto the next card in each hand
	add edi, TYPE CARD
LOOP readd

doneAdding:
	mov playerValue, al										; save new sums into playerValue and dealerValue
	mov dealerValue, bl

	pop ebx
	pop eax
	pop ecx
	pop edi
	pop esi
	ret 
addCards ENDP

;---------------------------------------------------------------------------------------------------
checkForAce PROC, 
	turn:BYTE
;	checks hand for an ace when they go over 21. Changes the ace's value to 1 
;---------------------------------------------------------------------------------------------------
.code
	push eax
	push esi
	push ecx

	mov ecx, 10													; 10 cards a hand
	.IF (turn == 1)												; point to Playerhand
		mov esi, OFFSET playerHand
	.ELSE														; point to Dealerhand
		mov esi, OFFSET dealerHand
	.ENDIF


searchThrough:
	mov al, (CARD PTR[esi]).Value										; finds first ace that is still 11 and changes it to 1
	.IF (al == 11)
		mov (CARD PTR[esi]).Value, 1
		call addCards												; Update values by calling addCards
		jmp donechecking
	.ENDIF
	add esi, TYPE CARD												; moves onto next card if not an ace with value 11
LOOP searchThrough													; Loops through all cards until an ace with value of 11 is found
donechecking:
	pop ecx
	pop esi
	pop eax
	ret
checkForAce ENDP

;---------------------------------------------------------------------------------------------------
dealerTurn PROC
;	does the dealer's turn after the player stands. Will hit up until 17 or until it beats player
;---------------------------------------------------------------------------------------------------
.code
	push eax												; to modify playerCash with playerBet
	push edx												; gotoxy and writestring
	mov playerTurn, 0										; set playerTurn to 0, uncovering dealer's hidden card
	mov dl, playerValue										; store playerValue into dl to be used for comparing
dealerturnagain:
	.WHILE (dealerValue < 17) || (dealerValue < dl)				; dealer must hit up to 17 or until it beats or matches player
		INVOKE Hit, dealer									; calls hit for dealer
		Invoke PrintHand, player								; displays both hands and values
		Invoke PrintHand, dealer
		call WaitMsg										; waitmessage for user
	.ENDW

	.IF (dealerValue > 21)									; dealer has bust, goes to check for ace
		INVOKE checkForAce, 0
		.IF (dealerValue < 21)
			jmp dealerturnagain								; if there was an ace, then it goes back to hitting
		.ENDIF
	.ENDIF
	
	call Clrscr
	Invoke PrintHand, player									; display cards
	Invoke PrintHand, dealer

	mov dh, 10
	mov dl, 20
	call Gotoxy
	mov dl, playerValue	
	.IF (dealerValue == dl)									; push, player bet is returned 
		mov edx, OFFSET pushMsg
		call WriteString
		mov dh, 11
		mov dl, 20
		call Gotoxy
		call WaitMsg
	.ELSEIF (dealerValue <= 21)								; dealer didn't bust and has higher value than player
		mov edx, OFFSET dealerWinsMsg							; dealer wins and player loses
		call WriteString
		mov dh, 11
		mov dl, 20
		call Gotoxy
		mov eax, playerBet
		sub playerCash, eax									; subtract playerBet from playerCash
		call WaitMsg
	.ELSE												; anything else means player won, dealer bust
		mov edx, OFFSET playerWinsMsg
		call WriteString
		mov dh, 11
		mov dl, 20
		call Gotoxy
		mov eax, playerBet
		add playerCash, eax									; add playerBet to playerCash
		call WaitMsg
	.ENDIF

	pop edx
	pop eax
	ret
dealerTurn ENDP

;---------------------------------------------------------------------------------------------------
Hit PROC, 
	turn:BYTE
;	adds a card to player or dealer's hand, depending on turn
;	will add up player and dealer's card values
;---------------------------------------------------------------------------------------------------
.code
	push edi												; used to retrieve cards from cardDeck
	mov edi, OFFSET cardDeck									; point to cards

	.IF (turn == 1)										; Player hits
		mov eax, 0										; used for adding to addresses
		mov al, BYTE PTR [esi]								; save number from card in number deck

		add edi, eax										; add four times because CARD is 4 bytes
		add edi, eax
		add edi, eax
		add edi, eax

		mov ax, (CARD PTR[edi]).Face							; copy Face
		mov (CARD PTR[edx]).Face, ax							; put card into Player hand by copying the card from deck into hand
		mov al, (CARD PTR[edi]).Suit							; copy suit
		mov (CARD PTR[edx]).Suit, al					
		mov al, (CARD PTR[edi]).Value							; copy value
		mov (CARD PTR[edx]).Value, al
		add playerValue, al									; adds the new cards value to playerValue
		add edx, TYPE CARD									; move onto next card for player
		inc esi											; move to next card in card numbers deck
	.ELSE												; Dealer Hits
		mov eax, 0
		mov al, BYTE PTR [esi]								; used for adding to addresses
	
		add edi, eax										; Same process as for player except for dealer now
		add edi, eax
		add edi, eax
		add edi, eax

		mov ax, (CARD PTR[edi]).Face							; copy Face
		mov (CARD PTR[ebx]).Face, ax							; put card into dealer hand
		mov al, (CARD PTR[edi]).Suit							; copy suit
		mov (CARD PTR[ebx]).Suit, al					
		mov al, (CARD PTR[edi]).Value							; copy value
		mov (CARD PTR[ebx]).Value, al
		add dealerValue, al									; add new card value to dealerValue
		add ebx, TYPE CARD									; move onto next card for player
		inc esi											; move to next card in card numbers deck
	.ENDIF
	pop edi
	ret
Hit ENDP

;---------------------------------------------------------------------------------------------------
ClearHands PROC
;	clears both hands after each round, sets all cards in both hands to 0
;---------------------------------------------------------------------------------------------------
.code
	push eax												; used to set the cards to 0
	push ecx												; used for loop
	mov ebx, OFFSET dealerHand								; point to dealerHand
	mov edx, OFFSET playerHand								; point to playerHand
	mov ecx, 10											; 10 cards each hand
clear:
	mov (CARD PTR[ebx]).Face,	ax							; set to 0 face, suit, value for both hands
	mov (CARD PTR[ebx]).Suit,	al
	mov (CARD PTR[ebx]).Value,	al
	mov (CARD PTR[edx]).Face,	ax
	mov (CARD PTR[edx]).Suit,	al
	mov (CARD PTR[edx]).Value,	al
	add ebx, TYPE CARD										; moves onto next card for both hands
	add edx, TYPE CARD
LOOP clear

	pop ecx
	pop eax
	ret 
ClearHands ENDP

;---------------------------------------------------------------------------------------------------
DealCards PROC 
;	deal cards to dealer and player, player is dealt first card, then dealer
;	modifies esi to keep track of which cards have been dealt out
;---------------------------------------------------------------------------------------------------
.code
	push eax												; used for adding to addresses 
	push edi												; points to cardDeck
	push ecx												; loop counter

	mov edi, OFFSET cardDeck									; point to cardDeck

	mov dealerValue, 0										; sets both value to 0 then adds the values as it prints out the cards
	mov playerValue, 0										; since dealcards is called before hitting, it will initialize both values to 0

	; esi is deckAsNum
	; ebx is dealerHand
	; edx is playerHand

	mov ecx, 2											; deal out two cards each
dealcardsloop:		
	mov eax, 0								
	mov al, BYTE PTR [esi]									; save number from card in number deck

	mov edi, OFFSET cardDeck
	add edi, eax											; add four times because CARD is 4 bytes
	add edi, eax
	add edi, eax
	add edi, eax

	mov ax, (CARD PTR[edi]).Face								; save Face
	mov (CARD PTR[edx]).Face, ax								; put card into Player hand by copying the card from deck into hand
	mov al, (CARD PTR[edi]).Suit								; suit
	mov (CARD PTR[edx]).Suit, al					
	mov al, (CARD PTR[edi]).Value								; value
	mov (CARD PTR[edx]).Value, al
	add playerValue, al										; add card value to playerValue
	add edx, TYPE CARD										; move onto next card for player
	inc esi												; move to next card in card numbers deck
	;------------------------Dealer turn-----------------------------------------------------------------------------------------
	mov eax, 0
	mov al, BYTE PTR [esi]									; take card number
	
	mov edi, OFFSET cardDeck
	add edi, eax
	add edi, eax
	add edi, eax
	add edi, eax

	mov ax, (CARD PTR[edi]).Face								; save Face
	mov (CARD PTR[ebx]).Face, ax								; put card into dealer hand
	mov al, (CARD PTR[edi]).Suit								; suit
	mov (CARD PTR[ebx]).Suit, al					
	mov al, (CARD PTR[edi]).Value								; value
	mov (CARD PTR[ebx]).Value, al
	add dealerValue, al
	add ebx, TYPE CARD										; move onto next card for player
	inc esi
LOOP dealcardsloop

	.IF (dealerValue > 21)									; when someone gets 2 aces, it changes an ace to value 1
		INVOKE checkForAce, 0
	.ELSEIF (playerValue > 21)
		INVOKE checkForAce, 1
	.ELSEIF (dealerValue == 21) && (playerValue == 21)			; when both get blackjack, it pushes
		push edx	
		mov edx, OFFSET pushMsg
		call WriteString
		mov dh, 11
		mov dl, 20
		call Gotoxy
		call WaitMsg
		pop edx
	.ELSEIF (dealerValue == 21)								; when dealer get BlackJack
		push edx
		mov dh, 10
		mov dl, 20
		call Gotoxy
		mov edx, OFFSET blackJackMsg
		call WriteString
		mov dh, 11
		mov dl, 20
		call Gotoxy
		mov edx, OFFSET dealerWinsMsg
		call WriteString
		mov eax, playerBet
		sub playerCash, eax
		pop edx
	.ELSEIF (playerValue == 21)								; when player gets blackjack
		push edx
		mov dh, 10
		mov dl, 20
		call Gotoxy
		mov edx, OFFSET blackJackMsg
		call WriteString
		mov dh, 11
		mov dl, 20
		call Gotoxy
		mov edx, OFFSET playerWinsMsg
		call WriteString
		mov eax, playerBet
		add playerCash, eax
		pop edx
	.ENDIF
	pop ecx
	pop edi
	pop eax
	ret
DealCards ENDP

;---------------------------------------------------------------------------------------------------
Shuffle PROC  
;	Shuffle deckAsNum by swapping numbers around
;---------------------------------------------------------------------------------------------------
.data
	firstCard DWORD ?										; save first random number for swapping
	secondCard DWORD ?										; second random number
.code
	push eax												; used for randomrange and swapping
	push ebx												; holding second card 
	push ecx												; loop counter
	push esi												; points to deckAsNum

	mov ecx, 200											; repeat 200 times to make it's fully shuffled
shufflecards:

	mov esi, OFFSET deckAsNum
	mov eax, 52											; get random cards to swap
	call RandomRange										; from irvine32.lib, generates a random number within range of 0 to whatever in eax	and puts it in eax	
	mov firstCard, eax										; save first random number into firstCard

	call Random32
	mov eax, 52											; 52 cards in a deck, so random number will be 0-51
	call RandomRange										; get second random number
	mov secondCard, eax										; save second random number into secondCard

	mov eax, 0											; zero out eax and ebx to use in swapping
	mov ebx, 0
	add esi, firstCard										; point to first random card
	mov al, [esi]											; saves that card into al
	mov esi, OFFSET deckAsNum
	add esi, secondCard										; point to second random card
	mov bl, [esi]											; saves that card into bl
	mov [esi], al											; puts first card into second card's place
	mov esi, OFFSET deckAsNum
	add esi, firstCard										; point back to first card
	mov [esi], bl											; puts second card into first card's place
LOOP shufflecards

	pop esi
	pop ecx
	pop ebx
	pop eax
	ret
Shuffle ENDP

;----------------------------------------------------------------------------------------------------
PrintHand PROC,
	turn:BYTE
;	need to move into eax 0 or 1 to indicate if dealer or player. Will skip appropriate 
;	number of rows depending on which
;----------------------------------------------------------------------------------------------------
.data
	rowsToSkip BYTE ?										; if its the player, start cards printout at row 19, start at 1 if dealer to save room for labels
.code
	push eax												; used in writeDec
	push ebx												; used to read card suit
	push ecx												; loop counter
	push edx												; used for gototxy and writestring
	push esi												; used to point to player or dealer hand
	
	mov ecx, 10											; up to 10 cards in each hand

	mov ebx, 0											; zero out eax and ebx for use	
	mov eax, 0											
	mov dl,0												; move to top left corner
	mov dh,0
	call Gotoxy											
	mov edx, OFFSET dealerHandMsg								; prints out dealerValue if playerTurn is over, else it's blank
	call WriteString
	.IF (playerTurn != 1)
		mov al, dealerValue
		call WriteDec
	.ENDIF
	mov dl,0
	mov dh,18
	call Gotoxy											; go to just above playerCards to display playerValue
	mov edx, OFFSET playerHandMsg
	call WriteString
	mov al, playerValue
	call WriteDec
	mov dl,0

	.IF (turn == 1)										; 1 indicates to print cards for player
		mov rowsToSkip, 19
		mov esi, OFFSET playerHand	
	.ELSEIF (playerTurn == 1)								; when still playerturn, cover the dealer's first card
		mov rowsToSkip, 1
		mov esi, OFFSET dealerHand
		jmp coverCard
	.ELSE												; when playerturn is over, show all dealer's cards
		mov rowsToSkip, 1
		mov esi, OFFSET dealerHand
	.ENDIF
testcard:
	mov ebx, 0
	mov dh, rowsToSkip										; move cursor to starting position for player or dealer
	mov bl, (CARD PTR[esi]).Suit
	.IF		(bl == 0)										; depending on suit of card, will jump to appropriate print out
		jmp retout										; when suit == 0, means that no more cards and will jmp to end
	.ELSEIF	(bl == 1)										; suit is heart
		jmp heart
	.ELSEIF	(bl == 2)										; suit is diamond
		jmp diamond
	.ELSEIF	(bl == 3)										; suit is clubs
		jmp clubs
	.ELSEIF	(bl == 4)										; suit is spades
		jmp spades
	.ELSE												; anything else, jump to end 
		jmp retout
	.ENDIF

heart:
	call Gotoxy											; goes to starting position for printing out cards, depending on player or dealer
	push edx
	mov edx, OFFSET cardline1								; print first cardline (top of card)
	call WriteString
	pop edx
	inc dh
	call Gotoxy											; move to next row
	push edx
	mov edx, OFFSET cardline2								; print second cardline
	call WriteString
	mov ax, (CARD PTR[esi]).Face								; get card's face to print out
	xchg al, ah											; xchg from book, swaps the values in the two registers
	call WriteChar
	xchg al, ah
	call WriteChar											; prints out card face
	mov edx, OFFSET cardline2a								; prints out rest of the row
	call WriteString
	pop edx
	inc dh
	call Gotoxy											; move onto next row
	push edx
	mov edx, OFFSET cardline3a								; print next line for heart
	call WriteString
	pop edx
	inc dh
	call Gotoxy											; move to next row
	push edx
	mov edx, OFFSET cardline4a								; print next line for heart
	call WriteString
	pop edx
	inc dh
	call Gotoxy											; move to next row
	push edx
	mov edx, OFFSET cardline5a								; print next line for heart
	call WriteString
	xchg al, ah
	call WriteChar
	xchg al, ah
	call WriteChar											; print out card face again
	mov edx, OFFSET cardline6								; print rest of row
	call WriteString
	pop edx
	inc dh
	call Gotoxy											; move to next row
	push edx
	mov edx, OFFSET cardline7								; print last part of card
	call WriteString
	pop edx
	add dl, 10											; moves onto next card printing by moving columns
	add esi, TYPE CARD										; move onto next card
	dec ecx												; decrement loop counter
jnz testcard												; jump back to testcard to see what suit the card has
jz  retout												; when it goes through loop 10 times, it jumps out. Should jump out before that anyways
diamond:
	call Gotoxy											; same as heart but now prints out for diamond
	push edx
	mov edx, OFFSET cardline1
	call WriteString
	pop edx
	inc dh
	call Gotoxy
	push edx
	mov edx, OFFSET cardline2
	call WriteString
	mov ax, (CARD PTR[esi]).Face
	xchg al, ah
	call WriteChar
	xchg al, ah
	call WriteChar
	mov edx, OFFSET cardline2b
	call WriteString
	pop edx
	inc dh
	call Gotoxy
	push edx
	mov edx, OFFSET cardline3b
	call WriteString
	pop edx
	inc dh
	call Gotoxy
	push edx
	mov edx, OFFSET cardline4b
	call WriteString
	pop edx
	inc dh
	call Gotoxy
	push edx
	mov edx, OFFSET cardline5b
	call WriteString
	xchg al, ah
	call WriteChar
	xchg al, ah
	call WriteChar
	mov edx, OFFSET cardline6
	call WriteString
	pop edx
	inc dh
	call Gotoxy
	push edx
	mov edx, OFFSET cardline7
	call WriteString
	pop edx
	add dl, 10
	add esi, TYPE CARD
	dec ecx
jnz testcard
jz  retout
clubs:													; same as heart but now prints out for clubs
	call Gotoxy
	push edx
	mov edx, OFFSET cardline1
	call WriteString
	pop edx
	inc dh
	call Gotoxy
	push edx
	mov edx, OFFSET cardline2
	call WriteString
	mov ax, (CARD PTR[esi]).Face
	xchg al, ah
	call WriteChar
	xchg al, ah
	call WriteChar
	mov edx, OFFSET cardline2c
	call WriteString
	pop edx
	inc dh
	call Gotoxy
	push edx
	mov edx, OFFSET cardline3c
	call WriteString
	pop edx
	inc dh
	call Gotoxy
	push edx
	mov edx, OFFSET cardline4c
	call WriteString
	pop edx
	inc dh
	call Gotoxy
	push edx
	mov edx, OFFSET cardline5c
	call WriteString
	xchg al, ah
	call WriteChar
	xchg al, ah
	call WriteChar
	mov edx, OFFSET cardline6
	call WriteString
	pop edx
	inc dh
	call Gotoxy
	push edx
	mov edx, OFFSET cardline7
	call WriteString
	pop edx
	add dl, 10
	add esi, TYPE CARD
	dec ecx
jnz testcard
jz  retout
spades:													; same as heart but now prints out for diamond
	call Gotoxy
	push edx
	mov edx, OFFSET cardline1
	call WriteString
	pop edx
	inc dh
	call Gotoxy
	push edx
	mov edx, OFFSET cardline2
	call WriteString
	mov ax, (CARD PTR[esi]).Face
	xchg al, ah
	call WriteChar
	xchg al, ah
	call WriteChar
	mov edx, OFFSET cardline2d
	call WriteString
	pop edx
	inc dh
	call Gotoxy
	push edx
	mov edx, OFFSET cardline3d
	call WriteString
	pop edx
	inc dh
	call Gotoxy
	push edx
	mov edx, OFFSET cardline4d
	call WriteString
	pop edx
	inc dh
	call Gotoxy
	push edx
	mov edx, OFFSET cardline5d
	call WriteString
	xchg al, ah
	call WriteChar
	xchg al, ah
	call WriteChar
	mov edx, OFFSET cardline6
	call WriteString
	pop edx
	inc dh
	call Gotoxy
	push edx
	mov edx, OFFSET cardline7
	call WriteString
	pop edx
	add dl, 10
	add esi, TYPE CARD
	dec ecx
jnz testcard
jz  retout
	
retout:												; When all cards are done printing,  skips a line then returns out
	call Crlf
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop eax
	ret

coverCard:											; For dealer's first card when it's still playerTurn
	mov dh, rowsToSkip									; prints out a blank card
	call Gotoxy
	push edx
	mov edx, OFFSET cardline1							
	call WriteString
	pop edx
	push ecx
	mov ecx, 4
blankcardloop:
	inc dh
	call Gotoxy
	push edx
	mov edx, OFFSET blankcard
	call WriteString
	pop edx
LOOP blankcardloop
	pop ecx
	inc dh
	call Gotoxy
	push edx
	mov edx, OFFSET cardline7
	call WriteString
	pop edx

	add dl, 10										; move onto next card position
	add esi, TYPE CARD									; move onto next card
	dec ecx											; decrement loop counter
	jnz testcard										; goes back to testcard

PrintHand ENDP


END main