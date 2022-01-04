using Plots
using DelimitedFiles, Statistics

# read in data
# cd("Model//7seg_stride//evaluation//")
fname = "7segsprint.1"
data = readdlm(fname, skipstart=8)
sim_time = data[:,1]

# plot
col = :black; lw = 2
anim = @animate for t ∈ eachindex(sim_time)
    # plot(dpi=100, size=(492, 276), aspect_ratio=:equal, legend=:none, grid=:off, border=:none)
    # plot(dpi=200, size=(492, 276), #=aspect_ratio=:equal,=# legend=:none, grid=:off, border=:none)
    plot(legend=:none, grid=:off, border=:none)
    xlims!(-0.8, 5.0)
    ylims!(-0.1, 5.7)
    plot!(data[t, 2:2:16], data[t,3:2:17], color=col, lw=lw) # Stance leg
    plot!(data[t,[12,18]], data[t,[13,19]], color=col, lw=lw) # Swing leg
    plot!(data[t,[4,8]], data[t,[5,9]], color=col, lw=lw) # Foot
    plot!([data[t,26]], [data[t,27]], st=:scatter, color=col, mc=:white, shape=:circle, ms=4, msw=2) # CoM
    hline!([0], color=:black)
    # title!("$(title): $(round(speed, digits=2)) m.s⁻¹")
end

gif(anim, "plot.gif", fps=60)

# plot(time, data[:,28:29], label=["vcmx" "vcmy"], legend=:outertopright)


## rmse 
f_exp = joinpath("C:\\Users\\tomro\\Desktop\\SprintingModel\\Model\\7seg_stride\\evaluation","matchingData2.csv")
data_exp = readdlm(f_exp, ',', Float64, '\n', skipstart=2)[:,3:2:end]
data_sim = readdlm("7segsprint.4", skipstart=8)[:,2:6]

# get matricies the same size and pad with zeros
if length(data_exp) > length(data_sim)
    temp = zeros(size(data_exp))
    temp[1:size(data_sim,1),:] = data_sim
    data_sim = deepcopy(temp)
end
# hat, hip, knee, ankle, mtp
mses = (data_sim - data_exp).^2 |> x -> mean(x, dims=1)
# cost = 10*mses[1]+sum(mses[2:4])+0.1*mses[5]
cost = sum(mses[2:4])

p1 = plot(data_exp[:,1:4], color=[1 2 3 4], label=["HAT" "hip" "knee" "ankle"], legend=:outertopright)
plot!(data_sim[:,1:4], color=[1 2 3 4 5], ls=:dash, label=:none, title="-- sim - exp, cost = $(round(cost))")
savefig(p1, "angles.png")

## torque generator plots
f = "7segsprint.7"
tqdata = readdlm(f , skipstart=8)

p2 = plot(sim_time, tqdata[:,5:2:7], label=["cc" "sec"], legend=:outertopleft, title="angular velocities")
p3 = plot(sim_time, tqdata[:,[2,8]], label=["he" "hf"], legend=:outertopright, title="torques")


## activation plot
f = "7segsprint.6"
act_data = readdlm(f, skipstart=8)
act_plts = []
labels = ["hip ext", "hip flx", "knee ext", "knee flx", "ankle ext", "ankle flx"]
for i in 2:7
    push!(act_plts, plot(act_data[:,1], act_data[:,i], label=labels[i-1], legend=:outertopright, ylims=(0,2.0)))
end
plot(act_plts..., layout=(3,2))

## compare torque
f1 = "noHAT_STR20_RMP010/7segsprint.3"
f2 = "noHAT_STR12_RMP010_PSSTQ/7segsprint.3"    # compare with passive torques or not
str20 = readdlm(f1, skipstart=8); str12 = readdlm(f2, skipstart=8)
labels=["hip" "knee" "ankle"]
plot(str20[:,1], str20[:,6:8], color=[1 2 3], label=labels.*"_str20", legend=:outertopright)
plot!(str12[:,1], str12[:,6:8], color=[1 2 3], ls=:dash, label=labels.*"_str12")


## compare activations
f1 = "noHAT_STR20_RMP010/7segsprint.6"
f2 = "noHAT_STR12_RMP010_PSSTQ/7segsprint.6"
str20 = readdlm(f1, skipstart=8); str12 = readdlm(f2, skipstart=8)
act_plts = []
labels = ["hip ext", "hip flx", "knee ext", "knee flx", "ankle ext", "ankle flx"]
for i in 2:7
    plt =  plot(str20[:,1], str20[:,i], label=labels[i-1], legend=:outertopright, ylims=(0,2.0), ls=:solid)
    plot!(str12[:,1], str12[:,i], label="strength 1.2")
    push!(act_plts, plt)

end
plot(act_plts..., layout=(3,2))
