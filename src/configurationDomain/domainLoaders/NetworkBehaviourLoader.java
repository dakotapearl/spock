package configurationDomain.domainLoaders;

import java.util.HashMap;

import configurationDomain.DomainConfig;
import configurationDomain.DomainConfigLoader;
import configurationDomain.configs.NetworkBehaviourConfig;

public class NetworkBehaviourLoader extends DomainConfigLoader {

	public NetworkBehaviourLoader(HashMap<String, DomainConfig> settings) {
		super(settings);
	}

	@Override
	public String getTag() {
		return "networkbehaviour";
	}

	@Override
	public DomainConfig newConfig(String type) {
		return new NetworkBehaviourConfig();
	}
	
	@Override
	public void setProperty(DomainConfig config, String property, String value) {
		if (config instanceof NetworkBehaviourConfig) {
			NetworkBehaviourConfig c = (NetworkBehaviourConfig) config;
			if (property.equals("name")) {
				c.name = value;
			} else if (property.equals("path")) {
				c.path = value;
			} else if (property.equals("fc")) {
				c.fcPath = value;
			} else if (property.equals("ts")) {
				c.tsPath = value;
			} else if (property.equals("tc")) {
				c.tcPath = value;
			} else if (property.equals("dp")) {
				c.dpPath = value;
			} else if (property.equals("ee")) {
				c.eePath = value;
			} else if (property.equals("lc")) {
				c.lcPath = value;
			} else if (property.equals("ip")) {
				c.ipPath = value;
			} else if (property.equals("op")) {
				c.opPath = value;
			} else if (property.equals("sp")) {
				c.spPath = value;
			} else if (property.equals("gs")) {
				c.gsPath = value;
			}
		} else {
			System.out.println("Error: wrong config class type passed to setProperty");
			System.exit(1);
		}
	}

}
