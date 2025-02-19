# money_model.py
import mesa
import random

def compute_gini(model): #gini close to 0 = eqauality, close to 1 = inequality
    agent_wealths = [agent.wealth for agent in model.agents]
    x = sorted(agent_wealths)
    n = model.num_agents
    B = sum(xi * (n - i) for i, xi in enumerate(x)) / (n * sum(x))
    return 1 + (1 / n) - 2 * B


class MoneyAgent(mesa.Agent):
    """An agent with fixed initial wealth."""

    def __init__(self, model, ethnicity):
        super().__init__(model)
        self.wealth = 1
        self.ethnicity = ethnicity
        self.steps_not_given = 0
    
    def give_money(self, similars):
        if self.wealth > 0:
            other_agent = self.random.choice(similars)
            other_agent.wealth += 1
            self.wealth -= 1
            self.steps_not_given = 0
        else:
            self.steps_not_given += 1

    def move(self):
        possible_steps = self.model.grid.get_neighborhood(
            self.pos, moore=True, include_center=False
        )
        new_position = self.random.choice(possible_steps)
        self.model.grid.move_agent(self, new_position)


class MoneyModel(mesa.Model):
    """A model with some number of agents."""

    def __init__(self, n, width, height):
        super().__init__()
        self.num_agents = n

        ethnicities = ["Green", "Blue", "Mixed"]

        self.agents_list = [
            MoneyAgent(self, ethnicity=self.random.choice(["Green", "Blue", "Mixed"]))
            for _ in range(n)
        ]
        # create the space
        self.grid = mesa.space.MultiGrid(width, height, True)
        self.running = True #loops the model using Mesa batch runner
        # collect the output
        self.datacollector = mesa.DataCollector(
            model_reporters={"Gini": compute_gini}, 
            agent_reporters={"Wealth": "wealth", "Ethnicity": "ethnicity", "Steps_not_given": "steps_not_given"}
        )

        # Create agents
        agents = self.agents_list  # Use the manually created agents list
        # Create x and y positions for agents
        x = self.rng.integers(0, self.grid.width, size=(n,))
        y = self.rng.integers(0, self.grid.height, size=(n,))
        for a, i, j in zip(agents, x, y):
            # Add the agent to a random grid cell
            self.grid.place_agent(a, (i, j))
    
    def step(self):
        self.datacollector.collect(self)

        grouped_agents = {}
        for agent in self.agents_list:  # Use self.agents_list instead of self.agents
            grouped_agents.setdefault(agent.ethnicity, []).append(agent)

        for ethnic, similars in grouped_agents.items():  # .items() to iterate correctly
            if ethnic != "Mixed":
                for agent in similars:
                    agent.give_money(similars)  # Call give_money correctly
            else:
                for agent in similars:
                    agent.give_money(self.agents_list)  # Give money to all agents
