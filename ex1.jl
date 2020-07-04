using LinearAlgebra

function main()
    
    n = 3
    A = rand(n,n)
    x = ones(n)
    b = A*x
    y = A\b
    println("Norm of error: ", norm(x-y))
    println("Norm of residual: ", norm(A*y-b))

end

main()
