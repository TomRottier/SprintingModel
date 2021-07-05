using Plots, DelimitedFiles, Dierckx, MAT
using Plots:PlotMeasures.px

# Load data
fnames = "optimisation/sprinter/10_0/7segsprint.1"
fnamec = "optimisation/college/9_1/7segsprint.1"
fnamee = "evaluation/evaluation/7segsprint.1"

fnames = [fnames, fnamec, fnamee]

# Data array
all_data = Array{Float64,3}(undef, 100, 37, 3)
sizes = Vector{Int64}(undef, 3)

# Plotting constants
lw = 3; col = :black

# Get data and generate GIFs
for (i, fname) in enumerate(fnames)
    # Read in data
    data = readdlm(fname, skipstart=8)
    name = split(fname, "/")[2]

    # Store data in array for later
    all_data[1:size(data, 1),1:size(data, 2),i] = data

    # Generate GIFs
    time = data[:,1]
    n = size(data, 1)
    sizes[i] = n

    anim = @animate for t ∈ 1:length(time)
        plot(dpi=100, size=(492, 276), aspect_ratio=:equal, legend=:none, grid=:off, border=:none)
        xlims!(-0.8, 2.1)
        ylims!(-0.1, 1.9)
        plot!(data[t, 2:2:16], data[t,3:2:17], color=col, lw=lw) # Stance leg
        plot!(data[t,[12,18]], data[t,[13,19]], color=col, lw=lw) # Swing leg
        plot!(data[t,[4,8]], data[t,[5,9]], color=col, lw=lw) # Foot
        plot!([data[t,26]], [data[t,27]], st=:scatter, color=col, mc=:white, shape=:circle, ms=4, msw=2) # CoM
        # hspan!([0,0], color=col)
    end

    gif(anim, "figures/" * name * ".gif", fps=60)

end


### Technique comparison 
N = 5   # Number of tiles per plot
plots = Vector(undef, 2)
tbase = 0:1:100
F = range(1, 101, length=N) |> collect |> x -> Int.(x)
lw = 7
all_data_norm = Array{Float64,3}(undef, 101, size(all_data, 2), 2)
for i ∈ 1:2
    # Normalise data
    n = sizes[i]
    data = all_data[1:n,:,i]
    time = data[:,1]
    tnorm = time ./ time[end] .* 100

    # Normalise data with cubic splines
    data_norm = Matrix{Float64}(undef, 101, size(data, 2))
    for (j, col) ∈ enumerate(eachcol(data))
        spl = Spline1D(tnorm, col)
        data_norm[:,j] = evaluate(spl, tbase)
    end
    data_norm[:,1] = tbase

    # Plot sequence of stills at timepoints F across the simulation
    plt = plot(aspect_ratio=:equal, legend=false, grid=:off, showaxis=false, ticks=false)
    for (k, f) ∈ enumerate(F)
        xlims!(-.9, 3.8)
        ylims!(-0.1, 1.9)
        c = (k - 1) * 0.7   # Add constant to each figure to space it out
        plot!(data_norm[f, 2:2:16] .+ c, data_norm[f,3:2:17], color=col, lw=lw) # Stance leg
        plot!(data_norm[f,[12,18]] .+ c, data_norm[f,[13,19]], color=col, lw=lw) # Swing leg
        plot!(data_norm[f,[4,8]] .+ c, data_norm[f,[5,9]], color=col, lw=lw) # Foot
        plot!([data_norm[f,26]] .+ c, [data_norm[f,27]], st=:scatter, color=col, mc=:white, shape=:circle, ms=9, msw=2) # CoM

    end
    plots[i] = plt

    # Save normalised data for later
    all_data_norm[:,:,i] = data_norm
    
end

# DPI:pixel width (Elsevier): 300:2244	500:3740	1000:7480
opt_plt = plot(plots..., layout=(2, 1), margin=0px, dpi=300, size=(2244, 1800))
savefig(opt_plt, "figures/optimisation.png")

# Side by side GIFs
lw = 2
anim = @animate for t ∈ 1:101
    plt1 = plot(aspect_ratio=:equal, legend=:none, grid=:off, border=:none)
    xlims!(-0.8, 1.0)
    # ylims!(-0.1, 1.9)
    plot!(all_data_norm[t, 2:2:16,1], all_data_norm[t,3:2:17,1], color=col, lw=lw) # Stance leg
    plot!(all_data_norm[t,[12,18],1], all_data_norm[t,[13,19],1], color=col, lw=lw) # Swing leg
    plot!(all_data_norm[t,[4,8],1], all_data_norm[t,[5,9],1], color=col, lw=lw) # Foot
    plot!([all_data_norm[t,26,1]], [all_data_norm[t,27,1]], st=:scatter, color=col, mc=:white, shape=:circle, ms=4, msw=2) # CoM

    plt2 = plot(aspect_ratio=:equal, legend=:none, grid=:off, border=:none)
    xlims!(-0.8, 1.0)
    # ylims!(-0.1, 1.9)
    plot!(all_data_norm[t, 2:2:16,2], all_data_norm[t,3:2:17,2], color=col, lw=lw) # Stance leg
    plot!(all_data_norm[t,[12,18],2], all_data_norm[t,[13,19],2], color=col, lw=lw) # Swing leg
    plot!(all_data_norm[t,[4,8],2], all_data_norm[t,[5,9],2], color=col, lw=lw) # Foot
    plot!([all_data_norm[t,26,2]], [all_data_norm[t,27,2]], st=:scatter, color=col, mc=:white, shape=:circle, ms=4, msw=2) # CoM

    plot(plt1, plt2, size=(492, 276), link=:all)
end

gif(anim, "figures/combined.gif", fps=60)

### Evaluation
# Get experimental data
exp_data = matread("C:/users/tomro/SprintingModel/Experimental data/data.mat")["dout"]["Average"]
points = exp_data["Markers"]["Data"]["Avg"][:,2:3,:]
names = vec(exp_data["Markers"]["Names"])
origin = points[:,:,(exp_data["Information"]["Leg"] * "TOE" .== names)]

# Points for plotting
rtoe = points[:,:,(names .== "RTOE")] - origin; 
ltoe = points[:,:,(names .== "LTOE")] - origin;
rmtp = points[:,:,(names .== "RMTP")] - origin;
lmtp = points[:,:,(names .== "LMTP")] - origin;
rhel = points[:,:,(names .== "RHEL")] - origin;
lhel = points[:,:,(names .== "LHEL")] - origin;
rajc = points[:,:,(names .== "RAJC")] - origin; 
lajc = points[:,:,(names .== "LAJC")] - origin;
rkjc = points[:,:,(names .== "RKJC")] - origin; 
lkjc = points[:,:,(names .== "LKJC")] - origin;
rhjc = points[:,:,(names .== "RHJC")] - origin; 
lhjc = points[:,:,(names .== "LHJC")] - origin;
rsjc = points[:,:,(names .== "RSJC")] - origin; 
lsjc = points[:,:,(names .== "LSJC")] - origin;
rejc = points[:,:,(names .== "REJC")] - origin; 
lejc = points[:,:,(names .== "LEJC")] - origin;
rwjc = points[:,:,(names .== "RWJC")] - origin; 
lwjc = points[:,:,(names .== "LWJC")] - origin;
ltjc = points[:,:,(names .== "LTJC")] - origin;
utjc = points[:,:,(names .== "UTJC")] - origin;
apex = points[:,:,(names .== "APEX")] - origin;
hjc = (rhjc + lhjc) ./ 2;

# CoM
wbcm = exp_data["CoM"]["Data"]["Avg"][:,2:3,1] - origin

# Simulation data
sim_data = all_data[1:sizes[3],:,3]
n = min(size(sim_data, 1), size(points, 1))

# Plot sequence of stills
exp_plt = plot(aspect_ratio=:equal, legend=false, grid=:off, showaxis=false, ticks=false)
T = range(1, n, length=N) |> collect |> x -> round.(x, digits=0) |> x -> Int.(x)
lw = 7
for (k, t) ∈ enumerate(T)
    xlims!(-.9, 3.8)
    ylims!(-0.1, 1.9)
    c = (k - 1) * 0.7   # Add constant to each figure to space it out

    plot!([rtoe[t,1], rmtp[t,1], rhel[t,1], rajc[t,1], rkjc[t,1], rhjc[t,1]] .+ c, [rtoe[t,2], rmtp[t,2], rhel[t,2], rajc[t,2], rkjc[t,2], rhjc[t,2]], color=col, lw=lw)
    plot!([rmtp[t,1], rajc[t,1]] .+ c, [rmtp[t,2], rajc[t,2]], color=col, lw=lw)
    plot!([ltoe[t,1], lmtp[t,1], lhel[t,1], lajc[t,1], lkjc[t,1], lhjc[t,1]] .+ c, [ltoe[t,2], lmtp[t,2], lhel[t,2], lajc[t,2], lkjc[t,2], lhjc[t,2]], color=col, lw=lw)
    plot!([lmtp[t,1], lajc[t,1]] .+ c, [lmtp[t,2], lajc[t,2]], color=col, lw=lw)
    plot!([hjc[t,1], ltjc[t,1], utjc[t,1], apex[t,1]] .+ c, [hjc[t,2], ltjc[t,2], utjc[t,2], apex[t,2]], color=col, lw=lw)
    plot!([rsjc[t,1], rejc[t,1], rwjc[t,1]] .+ c, [rsjc[t,2], rejc[t,2], rwjc[t,2]], color=col, lw=lw)
    plot!([lsjc[t,1], lejc[t,1], lwjc[t,1]] .+ c, [lsjc[t,2], lejc[t,2], lwjc[t,2]], color=col, lw=lw)
    plot!([wbcm[t,1]] .+ c, [wbcm[t,2]], st=:scatter, color=col, mc=:white, shape=:circle, ms=9, msw=2)
end

sim_plt = plot(aspect_ratio=:equal, legend=false, grid=:off, showaxis=false, ticks=false)
for (k, t) ∈ enumerate(T)
    xlims!(-.9, 3.8)
    ylims!(-0.1, 1.9)
    c = (k - 1) * 0.7   # Add constant to each figure to space it out

    plot!(sim_data[t, 2:2:16] .+ c, sim_data[t,3:2:17], color=col, lw=lw) # Stance leg
    plot!(sim_data[t,[12,18]] .+ c, sim_data[t,[13,19]], color=col, lw=lw) # Swing leg
    plot!(sim_data[t,[4,8]] .+ c, sim_data[t,[5,9]], color=col, lw=lw) # Foot
    plot!([sim_data[t,26]] .+ c, [sim_data[t,27]], st=:scatter, color=col, mc=:white, shape=:circle, ms=9, msw=2) # CoM
end

# DPI:pixel width (Elsevier): 300:2244	500:3740	1000:7480
eval_plt = plot(exp_plt, sim_plt, layout=(2, 1), margin=0px, dpi=300, size=(2244, 1800))
savefig(eval_plt, "figures/evaluation.png")
