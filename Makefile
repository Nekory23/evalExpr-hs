##
## EPITECH PROJECT, 2021
## EvalExpr
## File description:
## Makefile
##

BINARY_PATH 	:=	$(shell stack path --local-install-root)
BIN_NAME 		= 	funEvalExpr
NAME			= 	EvalExpr

all		:
			stack build
			@cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)
			@mv ./$(NAME) ./$(BIN_NAME)

tests_run:
			stack test

clean	:
			stack clean

fclean	:	clean
			rm -f $(BIN_NAME)

re		:	fclean all

.PHONY	:	all clean fclean re