package experimentDomain.TestExperiment.networkClasses;

import networkDomain.extensions.FiringCondition;

//TODO: implements timer
public class fc extends FiringCondition {
	
	public void refresh() {
		if (parent.transmissionContent.signalsRemain())
			setReadyToFire();
		else 
			setNotReadyToFire();
	}

	@Override
	public FiringCondition replicate() {
		// TODO Auto-generated method stub
		return null;
	}
	
}
