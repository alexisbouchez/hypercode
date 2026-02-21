import type { Lesson } from "../../types";

export const chaosSynchronization: Lesson = {
  id: "chaos-synchronization",
  title: "Chaos Synchronization",
  chapterId: "routes-to-chaos",
  content: `## Chaos Synchronization

It may seem paradoxical: two chaotic systems — which are exquisitely sensitive to initial conditions — can nonetheless **synchronize** completely, with their trajectories becoming identical after some transient time.

The key mechanism is **unidirectional coupling** (master-slave). A **master** system evolves freely; a **slave** system is nudged toward the master at each step with coupling strength k:

$$x_{n+1} = f(x_n) \\quad \\text{(master)}$$
$$y_{n+1} = (1 - k)\\,f(y_n) + k\\,f(x_n) \\quad \\text{(slave)}$$

When k = 0, the systems are independent and diverge (chaotic sensitivity). When k = 1, the slave is driven entirely by the master and synchronizes exactly after one step. For intermediate k, the slave is partially driven and may or may not synchronize depending on the Lyapunov exponent of the coupled system.

This phenomenon, discovered by Pecora and Carroll (1990), has applications in secure communications, where a message can be encoded in a chaotic signal and recovered by a synchronized receiver.

**Implement the following functions:**
- \`master_slave_logistic(x0, y0, r, k, steps)\` — evolve the master-slave coupled logistic maps for \`steps\` iterations, return (x, y)
- \`synchronization_error(x0, y0, r, k, n_transient, n_measure)\` — mean |x - y| over n_measure steps after n_transient transients
- \`is_synchronized(x0, y0, r, k, n_transient, n_measure, tol)\` — True if the mean sync error is below tol`,
  starterCode: `def master_slave_logistic(x0, y0, r, k, steps):
    # Master: x -> f(x) = r*x*(1-x)
    # Slave: y -> (1-k)*f(y) + k*f(x)
    pass

def synchronization_error(x0, y0, r, k, n_transient, n_measure):
    # Mean |x - y| over n_measure steps after n_transient transients
    pass

def is_synchronized(x0, y0, r, k, n_transient, n_measure, tol):
    # True if synchronization_error < tol
    pass`,
  solution: `def master_slave_logistic(x0, y0, r, k, steps):
    x, y = x0, y0
    for _ in range(steps):
        fx = r * x * (1.0 - x)
        fy = r * y * (1.0 - y)
        x_new = fx
        y_new = (1.0 - k) * fy + k * fx
        x, y = x_new, y_new
    return x, y

def synchronization_error(x0, y0, r, k, n_transient, n_measure):
    x, y = master_slave_logistic(x0, y0, r, k, n_transient)
    total = 0.0
    for _ in range(n_measure):
        fx = r * x * (1.0 - x)
        fy = r * y * (1.0 - y)
        x_new = fx
        y_new = (1.0 - k) * fy + k * fx
        x, y = x_new, y_new
        total += abs(x - y)
    return total / n_measure

def is_synchronized(x0, y0, r, k, n_transient, n_measure, tol):
    err = synchronization_error(x0, y0, r, k, n_transient, n_measure)
    return err < tol`,
  tests: [
    {
      name: "no coupling no sync",
      code: `{{FUNC}}\nprint(is_synchronized(0.1, 0.7, 4.0, 0.0, 500, 100, 1e-6))`,
      expected: `False\n`,
    },
    {
      name: "full coupling synchronizes",
      code: `{{FUNC}}\nprint(is_synchronized(0.1, 0.7, 4.0, 1.0, 200, 100, 1e-6))`,
      expected: `True\n`,
    },
    {
      name: "stronger coupling lower error",
      code: `{{FUNC}}\nerr_weak = synchronization_error(0.1, 0.7, 4.0, 0.1, 100, 50)\nerr_strong = synchronization_error(0.1, 0.7, 4.0, 0.8, 100, 50)\nprint(err_strong < err_weak)`,
      expected: `True\n`,
    },
  ],
};
