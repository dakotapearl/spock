
import metricDomain.MetricDomain;
import interfaceDomain.InterfaceDomain;
import tools.errorChecking.Log;
import dataDomain.DataDomain;
import environmentDomain.EnvironmentDomain;
import experimentDomain.Experiment;
import experimentDomain.ExperimentDomain;
import experimentDomain.Binary.BinaryAND;
import networkDomain.*;

/**
 * @author Loren Chorley
 */
public class MainSpock {
	
	public void initialise() {
		
		Log.write("Application (1): initialising domains");
		ExperimentDomain experimentDomain = new ExperimentDomain();
		EnvironmentDomain environmentDomain = new EnvironmentDomain();
		NetworkDomain networkDomain  = new NetworkDomain();
		DataDomain dataDomain = new DataDomain();
		MetricDomain metricDomain = new MetricDomain();
		InterfaceDomain interfaceDomain = new InterfaceDomain();
		
		Log.write("Application (2): connecting domains");
		// TODO write these methods
		experimentDomain.setEnvironmentDomain(environmentDomain);
		experimentDomain.setNetworkDomain(networkDomain);
		environmentDomain.setExperimentDomain(experimentDomain);
		environmentDomain.setNetworkDomain(networkDomain);
		environmentDomain.setDataDomain(dataDomain);
		environmentDomain.setMetricDomain(metricDomain);
		networkDomain.setEnvironmentDomain(environmentDomain);
		networkDomain.setExperimentDomain(experimentDomain);
		networkDomain.setDataDomain(dataDomain);
		networkDomain.setMetricDomain(metricDomain);
		interfaceDomain.setExperimentDomain(experimentDomain);
		interfaceDomain.setNetworkDomain(networkDomain);
		interfaceDomain.setEnvironmentDomain(environmentDomain);
		interfaceDomain.setDataDomain(dataDomain);
		interfaceDomain.setMetricDomain(metricDomain);
		
		Log.write("Application (3): initialising domains");
		environmentDomain.initialise();
		networkDomain.initialise();
		experimentDomain.initialise();
		dataDomain.initialise();
		interfaceDomain.initialise();
		metricDomain.initialise();
		
		Log.write("Application (4): selecting experiment");
		Experiment exp = new BinaryAND(experimentDomain);
		
		Log.write("Application (5): setting parameters");
		
		
		Log.write("Application (6): start threads");
		exp.start();
		
	}
	
	public static void main(String[] args) {

		Log.LoggingEnabled = true;
		Log.CreationLogsEnabled = true;
		Log.MechanismDebugEnabled = false;
		Log.TimeStampEnabled = true;
		
		new MainSpock().initialise();
		
		//TODO separate interface and running of experiments so that an experiment can be run multiple times.
		
	}
}
