package configurationDomain.configs;

import configurationDomain.DomainConfig;

public class NetworkBehaviourConfig implements DomainConfig {
	
	public String name = null;
	public String path = null;
	
	public String fcPath = null;
	public String tsPath = null;
	public String tcPath = null;
	public String dpPath = null;
	public String eePath = null;
	public String lcPath = null;
	
	public String ipPath = null;
	public String opPath = null;
	public String spPath = null;
	public String gsPath = null;
	
	public boolean isValid() {
		if (name != null && fcPath != null && tsPath != null && tcPath != null && dpPath != null && eePath != null && lcPath != null) {
			return true;
		} else {
			return false;
		}
	}
	
	public boolean filesExist() {
		//TODO test each file name passed in
		return true;
	}

	@Override
	public String getID() {
		return name;
	}
}
