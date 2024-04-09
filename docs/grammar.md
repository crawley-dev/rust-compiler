$$
\begin{align}

	[\text{Prog}] &\to 
		[\text{Stmt}]^* \\
	
	[\text{Stmt}] &\to 
	\begin{cases}
		\text{exit}([\text{Expr}]); \\
		\text{let}\space\text{ident} = [\text{Expr}]; \\
	\end{cases} \\

	[\text{Expr}] &\to 
	\begin{cases}
		% [\text{Term}] \\
		% [\text{BinExpr}] \\
		[\text{Expr}] \\
		[\text{Expr}] + [\text{Term}] \\
	\end{cases} \\

	[\text{Term}] &\to
	\begin{cases}
		\text{ident} \\
		\text{int\_lit} \\
	\end{cases} \\

	[\text{BinExpr}] &\to 
	\begin{cases}
		% [\text{Expr}] * [\tsext{Expr}] & \text{prec} = 1 \\
		% [\text{Expr}] / [\text{Expr}] & \text{prec} = 1 \\
		[\text{Expr}] + [\text{Expr}] & \text{prec} = 0 \\ 
		[\text{Expr}] - [\text{Expr}] & \text{prec} = 0 \\
	\end{cases} \\

\end{align}