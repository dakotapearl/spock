package networkDomain.behaviours.neuralEcosystem;

import java.util.Date;

import dataDomain.DataCell;
import networkDomain.extensions.EnergyEconomics;

/**
 * @author Loren Chorley
 */
public class NeuralEcosystem_EE extends EnergyEconomics {
	
	private double population = 1;
	private double growthRate = 1;
	private Date lastChecked;
	
	public NeuralEcosystem_EE() {
		lastChecked = new Date();
	}
	
	public double getPopulation() {
		//nano time keeper
		//get difference of lastChecked time and now = diff
		// population = population + (growthRate * diff)
		
		return population ;
	}
	
	// TODO linear increase, exponential decrease
	public void Event_ReceiveData(DataCell dataCell, double targetPopulation) {
		population += dataCell.getEnergy();
	}
	
	public void Event_SendData(DataCell dataCell, double targetPopulation) {
		population -= dataCell.getEnergy();
	}
	
	
	
	@Override
	public void run() {
		
		
	}

	@Override
	public EnergyEconomics replicate() {
		// TODO Auto-generated method stub
		return null;
	}

}
