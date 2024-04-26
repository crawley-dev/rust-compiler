$$
\begin{align}

	[\text{Prog}] &\to 
		[\text{Stmt}]^* \\
	
	[\text{Stmt}] &\to 
	\begin{cases}
		\text{exit}([\text{Expr}]); \\
		\text{let}\space\text{ident} = [\text{Expr}]; \\ 
		\text{if} ([\text{Expr}])[\text{Scope}] \\
		[\text{Scope}]
	\end{cases} \\ 

	[\text{Scope}] &\to 
		\{[\text{Stmt}]^*\} \\

	[\text{Expr}] &\to 
	\begin{cases}
		[\text{Expr}] \\
		[\text{BinExpr}] \\
		[\text{BoolExpr}] \\
	\end{cases} \\

	[\text{Term}] &\to
	\begin{cases}
		\text{ident} \\
		\text{int\_lit} \\
		([\text{Expr}]) \\
	\end{cases} \\

	[\text{BinExpr}] &\to 
	\begin{cases}
		[\text{Expr}] * [\text{Expr}] & \text{prec} = 11 \\
		[\text{Expr}] / [\text{Expr}] & \text{prec} = 11 \\
		[\text{Expr}] - [\text{Expr}] & \text{prec} = 10 \\
		[\text{Expr}] + [\text{Expr}] & \text{prec} = 10 \\ 
	\end{cases} \\

	[\text{BoolExpr}] &\to 
	\begin{cases}
		[\text{Expr}] & \text{!}  & [\text{Expr}] & \text{prec} = 13 \\
		[\text{Expr}] & \text{==} & [\text{Expr}] & \text{prec} = 8  \\
		[\text{Expr}] & \text{!=} & [\text{Expr}] & \text{prec} = 8  \\
		[\text{Expr}] & \text{<=} & [\text{Expr}] & \text{prec} = 8  \\ 
		[\text{Expr}] & \text{>=} & [\text{Expr}] & \text{prec} = 8  \\
		[\text{Expr}] & \text{<}  & [\text{Expr}] & \text{prec} = 8  \\
		[\text{Expr}] & \text{>}  & [\text{Expr}] & \text{prec} = 8  \\
		[\text{Expr}] & \text{OR} & [\text{Expr}] & \text{prec} = 3  \\
		[\text{Expr}] & \text{AND} & [\text{Expr}] & \text{prec} = 4  \\
	\end{cases} \\


\end{align}