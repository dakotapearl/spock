
import metricDomain.MetricDomain;
import interfaceDomain.InterfaceDomain;
import tools.errorChecking.Log;
import dataDomain.DataDomain;
import environmentDomain.EnvironmentDomain;
import experimentDomain.ExperimentDomain;
import networkDomain.*;

/**
 * @author Loren Chorley
 */
public class MainSpock {
	
	public void initialise() {
		
		Log.write("Application (1): instantiating domains");
		ExperimentDomain experimentDomain = new ExperimentDomain();
		EnvironmentDomain environmentDomain = new EnvironmentDomain();
		NetworkDomain networkDomain  = new NetworkDomain();
		DataDomain dataDomain = new DataDomain();
		MetricDomain metricDomain = new MetricDomain();
		InterfaceDomain interfaceDomain = new InterfaceDomain();
		
		Log.write("Application (2): connecting domains");
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
		
		// Serialised, so that preconditions can be set up
		// Note that start() methods are not serialised!
		// x.initialise() can be guarenteed to preceed x.start()
		// but nothing more.
		Log.write("Application (3): initialising domains");
		environmentDomain.initialise();
		networkDomain.initialise();
		experimentDomain.initialise();
		dataDomain.initialise();
		metricDomain.initialise();
		interfaceDomain.initialise();
		
		Log.write("Application (4): starting interface");
		interfaceDomain.start();
		
	}
	
	public static void main(String[] args) {

		Log.LoggingEnabled = true;
		Log.CreationLogsEnabled = false;
		Log.MechanismDebugEnabled = false;
		Log.TimeStampEnabled = true;
		Log.ThreadCreationEnabled = true;
		
		new MainSpock().initialise();
		
		//TODO separate interface and running of experiments so that an experiment can be run multiple times.
		
	}
}
