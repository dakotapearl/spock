package spock.network.behaviours;

import spock.network.core.NetworkNode;

/**
 * @author Loren Chorley
 */
public abstract class EnergyEconomics extends NetworkBehaviour<EnergyEconomics> {

    @Override
    public void replaceInNode(NetworkNode node, EnergyEconomics behaviour) {
        node.energyEconomics = behaviour;
    }
    
    private double energy = 0;

    public void offsetEnergy(double value) {
            energy = energy + value;
    }

    public double checkEnergy() {
            return energy;
    }

    //public abstract boolean continueFiring();
    //public abstract boolean hasExceededEnergyLimit();
    //public abstract double getNodeEfficiency(); // might require notify functions spread through NetworkNode and maybe other extensions
	
}
