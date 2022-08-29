import logging
from pathlib import Path
from typing import Union, Any

import geopandas
import geopandas as gpd
import pandas as pd
import pytz
import requests
import shapely.geometry
from joblib import Parallel, delayed
from shapely.geometry import shape
from tqdm.auto import tqdm

# Register `pandas.progress_apply` and `pandas.Series.map_apply` with `tqdm`
tqdm.pandas()


class BearerAuth(requests.auth.AuthBase):
    def __init__(self, token):
        self.token = token

    def __call__(self, r):
        r.headers["Authorization"] = f"Bearer {self.token}"
        return r


def localize_track_datetime(track, timestamp_name):
    """
    Transform datetime timestamps in Bike Citizens tracks into datetime64[ns]
    with the appropriate timezone.

    :param track: Bike Citizens track containing a timestamp with the name
        `timestamp_name` and a corresponding offset
        `{timestamp_name}_timezone_offset`
    :param timestamp_name: Name of the timestamp. Must have corresponding
        offset `{timestamp_name}_timezone_offset`
    :return: result of `to_numpy()` of localized Pandas timestamp
        (datetime64[ns])
    """
    timestamp = pd.to_datetime(track[timestamp_name])
    if timestamp.tzinfo is None:
        timestamp = timestamp.tz_localize(pytz.UTC)
    timestamp = timestamp.tz_convert(pytz.FixedOffset(track[f'{timestamp_name}_timezone_offset']))
    return timestamp.to_numpy()


def newest_file_path(folder_path: Path, suffix: str):
    files = list(folder_path.glob(f"*{suffix}"))
    return max(files, key=lambda x: x.stat().st_mtime)


def highest_folder_name(folder_path: Path):
    paths = sorted([d for d in folder_path.glob('*') if d.is_dir()],
                   key=lambda d: d.name, reverse=True)
    if paths:
        return paths[0].name
    else:
        return None


def concat_gdfs(gdfs: [gpd.GeoDataFrame]) -> gpd.GeoDataFrame:
    if gdfs:
        return gpd.GeoDataFrame(pd.concat(gdfs, ignore_index=True),
                                crs=gdfs[0].crs)
    else:
        return gpd.GeoDataFrame()


def restrict_tracks(
        gdf: geopandas.GeoDataFrame,
        must_intersect: tuple[shapely.geometry.base.BaseGeometry, Any] = None,
        must_be_within: tuple[shapely.geometry.base.BaseGeometry, Any] = None
) -> gpd.GeoDataFrame:
    """
    Restrict geometry in tracks GeoDataFrame by other geometry.

    :param gdf: A GeoDataFrame to be restricted
    :param must_intersect: Tuple of a geometry the geometries in `gdf` must
        intersect and that geometry's CRS (any value accepted by
        `pyproj.CRS.from_user_input()`)
    :param must_be_within: Tuple of a geometry the geometries in `gdf` must
        be within and that geometry's CRS (any value accepted by
        `pyproj.CRS.from_user_input()`)
    :return: Restricted copy of `gdf`
    """
    restricted_gdf = gdf.copy()
    # TODO parellelise
    if must_intersect:
        restricted_gdf = restricted_gdf[
            restricted_gdf.to_crs(must_intersect[1]).intersects(
                must_intersect[0])]
    if must_be_within:
        restricted_gdf = restricted_gdf[
            restricted_gdf.to_crs(must_be_within[1]).within(must_be_within[0])]
    return restricted_gdf


class DataHelper:
    """
    A helper class for handling data.
    """

    def __init__(self, data_folder_path: Union[str, Path], logger=None):
        """
        :param data_folder_path: Path to a directory containing folders for
            versions of the data set
        :param logger: Optional alternative logger
        """
        self.data_folder_path = Path(data_folder_path)
        if logger:
            self.logger = logger
        else:
            self.logger = logging.getLogger(__name__)
            self.logger.setLevel(logging.DEBUG)
            log_formatter = logging.Formatter("%(asctime)s \
                [%(levelname)-5.5s] %(message)s")
            c_handler = logging.StreamHandler()
            c_handler.setFormatter(log_formatter)
            c_handler.setLevel(logging.DEBUG)
            self.logger.addHandler(c_handler)

    BC_API_BASE_URL = "https://api.bikecitizens.net"
    BC_EXTERNAL_CAMPAIGNS_PATH = "api/v2/external_campaigns"
    SURVEY_EXPORT_SUFFIX = '_kyuc-export.csv'

    def __filepath_to_name_or_newest(self, subfolder, suffix, filename):
        subfolder_path = self.data_folder_path / subfolder
        if filename is None:
            filename = newest_file_path(subfolder_path, suffix)
        else:
            filename = Path(filename + suffix)
        return subfolder_path / filename

    def __load_csv(self, subfolder, filename, suffix, **kwargs):
        filepath = self.__filepath_to_name_or_newest(subfolder, suffix,
                                                     filename)
        return pd.read_csv(filepath, **kwargs)

    def __load_geopandas(self, subfolder, filename, suffix, **kwargs):
        filepath = self.__filepath_to_name_or_newest(subfolder, suffix,
                                                     filename)
        return gpd.read_file(filepath, **kwargs)

    def load_survey(self, version: str = None):
        """
        Load data from a KYUC survey.

        :param version: A folder in the data directory containing a file
            `{version}_kyuc-export.csv`. By default, the newest version (i.e.
            the first directory when ordering by name in descending order) is
            imported.
        :return: A Pandas DataFrame of the survey export.
        """
        return self.__load_csv(version, version, self.SURVEY_EXPORT_SUFFIX)

    def bc_request(self, path: str, token: str, tries: int = 3):
        """
        Make a request to the Bike Citizens external campaigns API.

        :param path: The sub path after the external campaigns API path
        :param token: Bearer authentication token
        :param tries: Number of retries if request or parsing fails
        :return: JSON response
        """
        url = f"{self.BC_API_BASE_URL}/" \
              f"{self.BC_EXTERNAL_CAMPAIGNS_PATH}/" \
              f"{path}"
        for i in range(1, tries + 1):
            try:
                response = requests.get(
                    url,
                    auth=BearerAuth(token),
                    headers={'Accept': 'application/json'}
                )
                return response.json()
            except Exception as ex:
                self.logger.debug(f'Error at try {i} for {url}', exc_info=ex)

    def get_tracks(self, token: str):
        """
        Get a list of tracks with information, but without geometry, for a
        token from the Bike Citizens external campaigns API.

        :param token: The token to request tracks for
        :return: A Pandas DataFrame containing the tracks related to the token
        """
        response = self.bc_request('tracks', token)
        tracks = response['data'] if 'data' in response else []
        tracks_df = pd.DataFrame(tracks)
        tracks_df['token'] = token
        if 'tags' in tracks_df.columns:
            tracks_df['tags'] = tracks_df['tags'].map(
                lambda tags: ';'.join([tag['name'] for tag in tags])
            )
        for time in [timecol for timecol in ['start_time', 'end_time']
                     if timecol in tracks_df.columns]:
            tracks_df[time] = tracks_df.apply(
                lambda t: localize_track_datetime(t, time),
                axis=1
            )
        return tracks_df

    def get_track_geometry(self, track_uuid: str, token: str):
        """
        Get a track's geometry in GeoJSON format from the Bike Citizens API.
        Geometry needs to be requested for each track individually.

        :param track_uuid: The track's UUID (contained in `/tracks` response)
        :param token: Corresponding authentication token
        :return: A `shapely` shape from the geometry part of GeoJSON response,
            or `None` if erroneous request
        """
        response_geojson = self.bc_request(f'tracks/{track_uuid}', token)
        if response_geojson and 'geometry' in response_geojson:
            return shape(response_geojson['geometry'])

    def get_tracks_with_geometry(self,
                                 tokens: [str],
                                 progress_bar: bool = False):
        """
        Get all tracks with geometry for a list of tokens from the Bike
        Citizens external campaigns API.

        :param tokens: List of tokens for which tracks should be requested
        :param progress_bar: Whether a progress bar for downloading track
            geometries should be displayed
        :return: GeoPandas GeoDataFrame containing all track information and
            geometry
        """
        tracks_df = pd.concat([self.get_tracks(token) for token in tokens])
        if progress_bar:
            tracks_df['geometry'] = tracks_df.progress_apply(
                lambda t: self.get_track_geometry(t.uuid, t.token),
                axis=1
            )
        else:
            tracks_df['geometry'] = tracks_df.apply(
                lambda t: self.get_track_geometry(t.uuid, t.token),
                axis=1
            )
        return gpd.GeoDataFrame(tracks_df, geometry=tracks_df.geometry)

    def save_tracks_per_token(self,
                              tracks_gdf: gpd.GeoDataFrame,
                              version: str,
                              subfolder: str = 'tracks'):
        """
        Save tracks to file, one GeoJSON file per user token within the
        provided sub folder. Files are named '{token}.geojson'.

        :param tracks_gdf: Tracks to be saved
        :param version: Name of version folder
        :param subfolder: Name of subfolder
        """
        folder_path = self.data_folder_path / version / subfolder
        folder_path.mkdir(parents=True, exist_ok=True)
        for token, gdf in tracks_gdf.groupby('token'):
            gdf.to_file(folder_path / f'{token}.geojson', driver='GeoJSON')

    def save_tracks_unifile(self,
                            tracks_gdf: gpd.GeoDataFrame,
                            version: str,
                            filename: str):
        """
        Save tracks to a single GeoJSON file.

        :param tracks_gdf: Tracks to be saved
        :param version: Name of version folder
        :param filename: File name to write to (without '.geojson')
        """
        # ensure version fields are written as strings
        tracks_gdf[['version', 'tags_version', 'name_version',
                    'is_favorite_version']] = tracks_gdf[
            ['version', 'tags_version', 'name_version',
             'is_favorite_version']].astype(str)

        folder_path = self.data_folder_path / version
        folder_path.mkdir(parents=True, exist_ok=True)
        tracks_gdf.to_file(folder_path / f'{filename}.geojson',
                           driver='GeoJSON')

    def load_tracks_unifile(self, version: str, filename: str)\
            -> gpd.GeoDataFrame:
        """
        Load tracks from single GeoJSON file.

        :param version: Name of version folder
        :param filename: Name of file to be read (without '.geojson')
        :return: GeoDataFrame containing contents of the file
        """
        return gpd.read_file(
            self.data_folder_path / version / f'{filename}.geojson'
        )

    def get_and_save_tracks_with_geometry(self, version: str, tokens: [str])\
            -> gpd.GeoDataFrame:
        """
        Download and save tracks with geometry for a list of tokens. Saved as
        one file per token in '{version}/tracks'. Parallelised.

        :param version: Name of version folder
        :param tokens: List of tokens for which tracks with geometries should
            be downloaded and saved
        :return: Combined GeoDataFrame of all tracks with geometries
        """
        def get_and_save_single(token):
            tracks = self.get_tracks_with_geometry([token])
            self.save_tracks_per_token(tracks, version)
            return tracks

        all_tracks = Parallel(n_jobs=-1)(
            delayed(get_and_save_single)(token) for token in tqdm(tokens)
        )
        return concat_gdfs(all_tracks)

    def load_tracks(
            self,
            version: str,
            subfolder: str = 'tracks',
            restrict_to: [str] = None
    ) -> gpd.GeoDataFrame:
        """
        Load track data from per-token GeoJSON files saved in the provided sub
        folder.

        :param version: Name of the version folder
        :param subfolder: Sub folder of the version folder containing track
            geojson files
        :param restrict_to: Only load tracks from tokens in this list. If
            `None`, load all tokens
        :return: A GeoDataFrame of loaded tracks
        """
        files = [d for d in (self.data_folder_path / version / subfolder).glob(
            '*.geojson')]
        if restrict_to:
            files = [f for f in files if f.stem in restrict_to]
        self.logger.info(f'Loading tracks from {len(files)} files')
        dfs = Parallel(n_jobs=-1)(
            delayed(gpd.read_file)(file) for file in tqdm(files)
        )
        tracks_df = concat_gdfs(dfs)
        for time in [timecol for timecol in ['start_time', 'end_time']
                     if timecol in tracks_df.columns]:
            tracks_df[time] = tracks_df.apply(
                lambda t: localize_track_datetime(t, time),
                axis=1
            )
        return tracks_df

    def load_or_download_tracks(self, version: str, tokens: [str])\
            -> gpd.GeoDataFrame:
        """
        Load existing tracks from file, download and save missing tracks for
        provided list of tokens.

        :param version: Name of version folder
        :param tokens: List of tokens for which tracks should be loaded from
            file or downloaded
        :return: A GeoDataFrame of all loaded or downloaded tracks
        """
        existing_tracks = self.load_tracks(version, restrict_to=tokens)
        if 'token' in existing_tracks.columns:
            existing_tokens = list(existing_tracks.token.unique())
        else:
            existing_tokens = []
        missing = [t for t in tokens if t not in existing_tokens]

        self.logger.info(
            f'Missing {len(missing)} out of {len(tokens)} tokens, starting download...')
        missing_tracks = self.get_and_save_tracks_with_geometry(version,
                                                                missing)
        return concat_gdfs([existing_tracks, missing_tracks])

    def load_city_counter_data(self,
                               subfolder: str = 'counters',
                               filename: str = None) -> pd.DataFrame:
        """
        Load data from city cycling counters. Retrieved from
        https://www.data.gv.at/katalog/dataset/stadt-wien_radverkehrszhlungenderstadtwien

        :param subfolder: Sub folder in which the survey is saved
        :param filename: File name without `.csv` of a file to be loaded. By
            default, the newest file (i.e. the one with the highest 'modified
            at' date) is imported
        :return: A Pandas DataFrame
        """
        return self.__load_csv(subfolder, filename, '.csv',
                               sep=';', encoding='latin-1')

    def load_tracks_counter_data(self,
                                 version: str,
                                 filename: str,
                                 **kwargs) -> pd.DataFrame:
        """
        :param version: Version sub folder
        :param filename: File name without `.csv` of a file to be loaded.
        :return: A Pandas DataFrame
        """
        df = self.__load_csv(version, filename, '.csv', index_col=[0],
                             **kwargs)
        df.index = pd.to_datetime(df.index)
        return df
