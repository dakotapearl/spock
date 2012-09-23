
import interfaceDomain.InterfaceDomain;
import tools.Log;
import dataDomain.DataDomain;
import environmentDomain.EnvironmentDomain;
import experimentDomain.ExperimentDomain;
import networkDomain.*;

public class MainSpock {
	
	public void initialise() {
		
		Log.write("Application (1): initialising domains");
		ExperimentDomain experimentDomain = new ExperimentDomain();
		EnvironmentDomain environmentDomain = new EnvironmentDomain();
		NetworkDomain networkDomain  = new NetworkDomain();
		DataDomain dataDomain = new DataDomain();
		InterfaceDomain interfaceDomain = new InterfaceDomain();
		
		Log.write("Application (2): connecting domains");
		// TODO write these methods
		experimentDomain.setEnvironmentDomain(environmentDomain);
		experimentDomain.setNetworkDomain(networkDomain);
		environmentDomain.setExperimentDomain(experimentDomain);
		environmentDomain.setNetworkDomain(networkDomain);
		environmentDomain.setDataDomain(dataDomain);
		networkDomain.setEnvironmentDomain(environmentDomain);
		networkDomain.setExperimentDomain(experimentDomain);
		networkDomain.setDataDomain(dataDomain);
		interfaceDomain.setExperimentDomain(experimentDomain);
		interfaceDomain.setNetworkDomain(networkDomain);
		interfaceDomain.setEnvironmentDomain(environmentDomain);
		interfaceDomain.setDataDomain(dataDomain);
		
		Log.write("Application (3): selecting experiment");
		//experimentsDomain.setExperiment(ExperimentsDomain.XID_TEXTUAL_ONE);
		
		Log.write("Application (4): initialising domains");
		environmentDomain.initialise();
		networkDomain.initialise();
		experimentDomain.initialise();
		dataDomain.initialise();
		
		Log.write("Application (5): setting parameters");
		
		
		Log.write("Application (6): start threads");
		// TODO
		
	}
	
	public static void main(String[] args) {

		Log.LoggingEnabled = true;
		Log.CreationLogsEnabled = true;
		Log.MechanismDebugEnabled = true;
		Log.TimeStampEnabled = true;
		
		new MainSpock().initialise();
		
		//TODO separate interface and running of experiments so that an experiment can be run multiple times.
		
	}
}
