import main
import gleam/io
import gleam/int

pub fn main() {
  // For now, let's provide a simple way to test different algorithms
  // Since command line parsing is complex in Gleam, we'll use a different approach
  
  io.println("=== Algorithm Selection ===")
  io.println("To test different algorithms, edit the function call below:")
  io.println("Current: Running push-sum analysis as requested")
  io.println("")
  
  // Run push-sum analysis as requested by the user
  main.execute_comprehensive_analysis(1000, "full", "push-sum")
}
