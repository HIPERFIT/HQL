# Plot comparision graph of compounding methods

# Settings
set key left noreverse enhanced autotitles box linetype -1 linewidth 1.000

# Line types
#set style func linespoints

# Plot specific variables
r0 = 0.03
r1 = 0.05
r2 = 0.10

years = 10

# Terminal and ranges
set size 1.0,0.618; set term post eps monochrome "Helvetica, 14"
set terminal postscript enhanced monochrome dashed
#set terminal qt

set xrange[0:years]
set yrange[1:exp(r2*years)*1.05]

# Custom formatter for interest rates
formatir(r) = sprintf("%2.f%% interest", r*100)

# Set title
#set title "Continuous compounding with various rates" font "Helvetica bold, 18"

# Labels
set xlabel "Time (years)" font "bold"
set ylabel "Rate (percent)" font "bold"

f(x) = (1+r2)**x

#f(x) = 

set sample 11

# Plot and write to file
set output "comp02.eps"
set grid
plot \
exp(r2*x) ti "Continuous" lw 3, \
f(x) ti "Discrete" lw 3 w steps, \
1+x*r2 ti "Simple" lw 3 lt 5


