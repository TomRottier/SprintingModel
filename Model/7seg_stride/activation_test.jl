using Plots

function activation(t, p, n)
    t0 = 0.0
    tr = 0.0
    a = NaN64
    for i in 1:n
        j = (i - 1) * 3
        a0 = p[j + 1]
        t0 = t0 + tr + p[j + 2]
        tr = p[j + 3]
        a1 = p[j + 4]

        tr < 0.01 && (tr = 0.01)
        afun = a0 + (a1 - a0) * ((t - t0) / tr)^3 * (6 * ((t - t0) / tr)^2 - 15 * ((t - t0) / tr) + 10)

        if t ≤ t0
            return a0
        elseif ((t ≥ t0) & (t ≤ t0 + tr))
            return afun
        elseif t ≥ t0 + tr
            a = a1
        end
    end

    return a
end

# parameters
p = [0.0,0.0,0.1,1.0,0.0,0.1,0.0,0.1,0.2,0.5,0.1,0.1,1.0]
n = 4
time = 0:0.001:0.7

a = [activation(t, p, n) for t in time]
plot(time, a, legend=:none)