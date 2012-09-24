package experimentDomain.TestExperiment.networkClasses;

import networkDomain.extensions.FiringCondition;

//TODO: implements timer
public class fc extends FiringCondition {
	
	public void update() {
		if (parent.transmissionContent.signalsRemain())
			setReadyToFire();
		else 
			setNotReadyToFire();
	}
	
}
