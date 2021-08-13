using Plots
using DelimitedFiles

# read in data
fname = "base\\7segsprint.1"
data = readdlm(fname, skipstart=8)
time = data[:,1]

# plot
col = :black; lw = 2
anim = @animate for t ∈ eachindex(time)
    # plot(dpi=100, size=(492, 276), aspect_ratio=:equal, legend=:none, grid=:off, border=:none)
    # plot(dpi=200, size=(492, 276), #=aspect_ratio=:equal,=# legend=:none, grid=:off, border=:none)
    plot(legend=:none, grid=:off, border=:none)
    xlims!(-0.8, 5.0)
    ylims!(-0.1, 5.7)
    plot!(data[t, 2:2:16], data[t,3:2:17], color=col, lw=lw) # Stance leg
    plot!(data[t,[12,18]], data[t,[13,19]], color=col, lw=lw) # Swing leg
    plot!(data[t,[4,8]], data[t,[5,9]], color=col, lw=lw) # Foot
    plot!([data[t,26]], [data[t,27]], st=:scatter, color=col, mc=:white, shape=:circle, ms=4, msw=2) # CoM
    # title!("$(title): $(round(speed, digits=2)) m.s⁻¹")
end

gif(anim, "base//plot.gif", fps=60)

# plot(time, data[:,28:29], label=["vcmx" "vcmy"], legend=:outertopright)